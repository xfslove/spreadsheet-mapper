package spread.sheet.w2o.processor;

import spread.sheet.model.core.Cell;
import spread.sheet.model.core.Row;
import spread.sheet.model.core.Sheet;
import spread.sheet.model.meta.FieldMeta;
import spread.sheet.model.meta.SheetMeta;
import spread.sheet.w2o.processor.listener.*;
import spread.sheet.w2o.setter.BeanUtilsValueSetter;
import spread.sheet.w2o.setter.FieldValueSetter;
import spread.sheet.w2o.setter.ValueSetter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * sheet to objects processor
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class DefaultSheetProcessor<T> implements SheetProcessor<T> {

  private Sheet sheet;

  private SheetMeta sheetMeta;

  private ObjectFactory<T> objectFactory;

  private SheetProcessorListener<T> sheetProcessorListener = new NoopSheetProcessorListener<>();

  private RowProcessorListener<T> rowProcessorListener = new NoopRowProcessorListener<>();

  private CellProcessorListener<T> cellProcessorListener = new NoopCellProcessorListener<>();

  private Map<String, FieldValueSetter> key2fieldValueSetter = new HashMap<>();

  private ValueSetter defaultValueSetter = new BeanUtilsValueSetter();

  @Override
  public SheetProcessor<T> fieldValueSetter(FieldValueSetter... fieldValueSetters) {
    if (fieldValueSetters == null) {
      return this;
    }

    for (FieldValueSetter setter : fieldValueSetters) {
      key2fieldValueSetter.put(setter.getMatchField(), setter);
    }
    return this;
  }

  @Override
  public SheetProcessor<T> objectFactory(ObjectFactory<T> objectFactory) {
    this.objectFactory = objectFactory;
    return this;
  }

  @Override
  public SheetProcessor<T> sheetProcessorListener(SheetProcessorListener<T> sheetProcessorListener) {
    this.sheetProcessorListener = sheetProcessorListener;
    return this;
  }

  @Override
  public SheetProcessor<T> rowProcessorListener(RowProcessorListener<T> rowProcessorListener) {
    this.rowProcessorListener = rowProcessorListener;
    return this;
  }

  @Override
  public SheetProcessor<T> cellProcessorListener(CellProcessorListener<T> cellProcessorListener) {
    this.cellProcessorListener = cellProcessorListener;
    return this;
  }

  @Override
  public SheetProcessor<T> sheet(Sheet sheet) {
    this.sheet = sheet;
    return this;
  }

  @Override
  public SheetProcessor<T> sheetMeta(SheetMeta sheetMeta) {
    this.sheetMeta = sheetMeta;
    return this;
  }

  @Override
  public List<T> process() {
    if (sheet == null) {
      throw new WorkbookProcessException("set sheet first");
    }

    if (sheetMeta == null) {
      throw new WorkbookProcessException("set sheet meta first");
    }

    if (objectFactory == null) {
      throw new WorkbookProcessException("set object factory first");
    }

    List<FieldMeta> fieldMetas = sheetMeta.getFieldMetas();
    Map<Integer, FieldMeta> columnIndex2fieldMeta = buildFieldMetaMap(fieldMetas);

    List<T> oneSheetObjects = new ArrayList<>();
    sheetProcessorListener.before(sheet, sheetMeta);

    for (int i = sheetMeta.getDataStartRowIndex(); i <= sheet.sizeOfRows(); i++) {
      Row row = sheet.getRow(i);

      T object = objectFactory.create(row);

      rowProcessorListener.before(row, sheetMeta, object);

      for (Cell cell : row.getCells()) {

        FieldMeta fieldMeta = columnIndex2fieldMeta.get(cell.getIndex());

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