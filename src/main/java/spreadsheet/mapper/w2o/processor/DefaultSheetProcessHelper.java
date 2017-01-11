package spreadsheet.mapper.w2o.processor;

import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.w2o.factory.ObjectFactory;
import spreadsheet.mapper.w2o.setter.BeanUtilsValueSetter;
import spreadsheet.mapper.w2o.setter.FieldValueSetter;
import spreadsheet.mapper.w2o.setter.ValueSetter;
import spreadsheet.mapper.w2o.listener.*;

import java.util.*;

/**
 * sheet to objects processor
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class DefaultSheetProcessHelper<T> implements SheetProcessHelper<T> {

  private Sheet sheet;

  private SheetMeta sheetMeta;

  private ObjectFactory<T> objectFactory;

  private SheetProcessListener<T> sheetProcessListener = new NoopSheetProcessListener<>();

  private RowProcessListener<T> rowProcessListener = new NoopRowProcessListener<>();

  private CellProcessListener<T> cellProcessListener = new NoopCellProcessListener<>();

  private Map<String, FieldValueSetter<T>> key2fieldValueSetter = new LinkedHashMap<>();

  private ValueSetter<T> defaultValueSetter = new BeanUtilsValueSetter<>();

  @Override
  @SuppressWarnings("unchecked")
  public SheetProcessHelper<T> fieldValueSetter(FieldValueSetter<T>... fieldValueSetters) {
    if (fieldValueSetters == null) {
      return this;
    }

    for (FieldValueSetter<T> setter : fieldValueSetters) {
      key2fieldValueSetter.put(setter.getMatchField(), setter);
    }
    return this;
  }

  @Override
  public SheetProcessHelper<T> objectFactory(ObjectFactory<T> objectFactory) {
    this.objectFactory = objectFactory;
    return this;
  }

  @Override
  public SheetProcessHelper<T> sheetProcessorListener(SheetProcessListener<T> sheetProcessListener) {
    this.sheetProcessListener = sheetProcessListener;
    return this;
  }

  @Override
  public SheetProcessHelper<T> rowProcessorListener(RowProcessListener<T> rowProcessListener) {
    this.rowProcessListener = rowProcessListener;
    return this;
  }

  @Override
  public SheetProcessHelper<T> cellProcessorListener(CellProcessListener<T> cellProcessListener) {
    this.cellProcessListener = cellProcessListener;
    return this;
  }

  @Override
  public SheetProcessHelper<T> sheet(Sheet sheet) {
    this.sheet = sheet;
    return this;
  }

  @Override
  public SheetProcessHelper<T> sheetMeta(SheetMeta sheetMeta) {
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
    sheetProcessListener.before(sheet, sheetMeta);

    for (int i = sheetMeta.getDataStartRowIndex(); i <= sheet.sizeOfRows(); i++) {
      Row row = sheet.getRow(i);

      T object = objectFactory.create(row);

      rowProcessListener.before(row, sheetMeta, object);

      for (Cell cell : row.getCells()) {

        FieldMeta fieldMeta = columnIndex2fieldMeta.get(cell.getIndex());

        if (fieldMeta == null) {
          // if missing field meta skip the cell(same column index with field meta)
          continue;
        }

        cellProcessListener.before(cell, fieldMeta, object);

        FieldValueSetter<T> fieldValueSetter = key2fieldValueSetter.get(fieldMeta.getName());

        if (fieldValueSetter != null) {
          fieldValueSetter.set(object, cell, fieldMeta);
        } else {

          defaultValueSetter.set(object, cell, fieldMeta);
        }

        cellProcessListener.after(cell, fieldMeta, object);

      }

      rowProcessListener.after(row, sheetMeta, object);

      oneSheetObjects.add(object);
    }

    sheetProcessListener.after(sheet, sheetMeta, oneSheetObjects);

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