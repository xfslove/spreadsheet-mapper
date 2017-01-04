package spread.sheet.w2o.processor;

import spread.sheet.model.core.SheetList;
import spread.sheet.model.core.Cell;
import spread.sheet.model.core.Row;
import spread.sheet.model.core.Sheet;
import spread.sheet.model.meta.FieldMeta;
import spread.sheet.model.meta.SheetMeta;
import spread.sheet.w2o.processor.listener.*;
import spread.sheet.w2o.setter.BeanUtilValueSetter;
import spread.sheet.w2o.setter.FieldValueSetter;
import spread.sheet.w2o.setter.ValueSetter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * sheet to objects processor
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class DefaultSheetProcessor implements SheetProcessor {

  private Sheet sheet;

  private SheetMeta sheetMeta;

  private ObjectFactory objectFactory;

  private SheetProcessorListener sheetProcessorListener = new NoopSheetProcessorListener();

  private RowProcessorListener rowProcessorListener = new NoopRowProcessorListener();

  private CellProcessorListener cellProcessorListener = new NoopCellProcessorListener();

  private Map<String, FieldValueSetter> key2fieldValueSetter = new HashMap<>();

  private ValueSetter defaultValueSetter = new BeanUtilValueSetter();

  @Override
  public SheetProcessor fieldValueSetter(FieldValueSetter... setters) {
    if (setters == null) {
      return this;
    }

    for (FieldValueSetter setter : setters) {
      key2fieldValueSetter.put(setter.getMatchField(), setter);
    }
    return this;
  }

  @Override
  public SheetProcessor objectFactory(ObjectFactory objectFactory) {
    this.objectFactory = objectFactory;
    return this;
  }

  @Override
  public SheetProcessor sheetProcessorListener(SheetProcessorListener sheetProcessorListener) {
    this.sheetProcessorListener = sheetProcessorListener;
    return this;
  }

  @Override
  public SheetProcessor rowProcessorListener(RowProcessorListener rowProcessorListener) {
    this.rowProcessorListener = rowProcessorListener;
    return this;
  }

  @Override
  public SheetProcessor cellProcessorListener(CellProcessorListener cellProcessorListener) {
    this.cellProcessorListener = cellProcessorListener;
    return this;
  }

  @Override
  public SheetProcessor sheet(Sheet sheet) {
    this.sheet = sheet;
    return this;
  }

  @Override
  public SheetProcessor sheetMeta(SheetMeta sheetMeta) {
    this.sheetMeta = sheetMeta;
    return this;
  }

  @Override
  public SheetList<Object> process() {
    if (sheet == null) {
      throw new WorkbookProcessException("set sheet first");
    }

    if (sheetMeta == null) {
      throw new WorkbookProcessException("set sheet meta first");
    }

    if (objectFactory == null) {
      throw new WorkbookProcessException("set object factory first");
    }

    if (sheet.getIndex() != sheetMeta.getSheetIndex()) {
      throw new WorkbookProcessException("sheet meta[sheet index:" + sheetMeta.getSheetIndex() + "] not belong to the sheet[index:" + sheet.getIndex() + "]");
    }

    List<FieldMeta> fieldMetas = sheetMeta.getFieldMetas();
    Map<Integer, FieldMeta> columnIndex2fieldMeta = buildFieldMetaMap(fieldMetas);

    SheetList<Object> oneSheetObjects = new SheetList<>(sheet.getIndex());
    sheetProcessorListener.before(sheet, sheetMeta);

    for (int i = sheetMeta.getDataStartRowIndex(); i <= sheet.sizeOfRows(); i++) {
      Row row = sheet.getRow(i);

      Object object = objectFactory.create(row);

      rowProcessorListener.before(row, sheetMeta, object);

      for (Cell cell : row.getCells()) {

        FieldMeta fieldMeta = columnIndex2fieldMeta.get(cell.getColumnIndex());

        if (fieldMeta == null) {
          // if missing field meta skip the cell(same column index with field meta)
          continue;
        }

        cellProcessorListener.before(cell, fieldMeta, object);

        FieldValueSetter fieldValueSetter = key2fieldValueSetter.get(fieldMeta.getName());

        if (fieldValueSetter != null) {
          fieldValueSetter.set(object, cell, fieldMeta);
        } else {

          defaultValueSetter.set(object, cell, fieldMeta);
        }

        cellProcessorListener.after(cell, fieldMeta, object);

      }

      rowProcessorListener.after(row, sheetMeta, object);

      oneSheetObjects.add(object);
    }

    sheetProcessorListener.after(sheet, sheetMeta, oneSheetObjects);

    return oneSheetObjects;
  }

  private Map<Integer, FieldMeta> buildFieldMetaMap(List<FieldMeta> fieldMetas) {
    Map<Integer, FieldMeta> columnIndex2fieldMeta = new HashMap<>();
    for (FieldMeta fieldMeta : fieldMetas) {
      columnIndex2fieldMeta.put(fieldMeta.getColumnIndex(), fieldMeta);
    }
    return columnIndex2fieldMeta;
  }
}