package extensible.sheet.w2o.processor;

import extensible.sheet.model.core.Cell;
import extensible.sheet.model.core.Row;
import extensible.sheet.model.core.Sheet;
import extensible.sheet.model.meta.FieldMeta;
import extensible.sheet.model.meta.SheetMeta;
import extensible.sheet.w2o.processor.listener.*;
import extensible.sheet.w2o.setter.BeanUtilValueSetter;
import extensible.sheet.w2o.setter.FieldValueSetter;
import extensible.sheet.w2o.setter.ValueSetter;

import java.util.ArrayList;
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
  public List<Object> process() {
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
    Map<Integer, FieldMeta> columnIndex2fieldMeta = new HashMap<>();
    for (FieldMeta fieldMeta : fieldMetas) {
      columnIndex2fieldMeta.put(fieldMeta.getColumnIndex(), fieldMeta);
    }

    List<Object> oneSheetObjects = new ArrayList<>();
    sheetProcessorListener.before(sheet, sheetMeta);

    for (int i = sheetMeta.getDataStartRowIndex(); i <= sheet.sizeOfRows(); i++) {
      Row row = sheet.getRow(i);

      Object object = objectFactory.create(row);

      rowProcessorListener.before(row, sheetMeta, object);

      for (int j = 1; j <= row.sizeOfCells(); j++) {

        FieldMeta fieldMeta = columnIndex2fieldMeta.get(i);
        Cell cell = row.getCell(j);

        FieldValueSetter fieldValueSetter = key2fieldValueSetter.get(fieldMeta.getName());

        cellProcessorListener.before(cell, fieldMeta, object);

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

}