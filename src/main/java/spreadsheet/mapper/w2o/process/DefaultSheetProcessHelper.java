package spreadsheet.mapper.w2o.process;

import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.w2o.process.factory.ObjectFactory;
import spreadsheet.mapper.w2o.process.setter.BeanUtilsSetter;
import spreadsheet.mapper.w2o.process.setter.FieldSetter;
import spreadsheet.mapper.w2o.process.setter.Setter;
import spreadsheet.mapper.w2o.process.listener.*;

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

  private Map<String, FieldSetter<T>> field2setter = new LinkedHashMap<>();

  private Setter<T> defaultSetter = new BeanUtilsSetter<>();

  @Override
  @SuppressWarnings("unchecked")
  public SheetProcessHelper<T> fieldSetters(FieldSetter<T>... fieldSetters) {
    if (fieldSetters == null) {
      return this;
    }

    for (FieldSetter<T> setter : fieldSetters) {
      field2setter.put(setter.getMatchField(), setter);
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

        FieldSetter<T> fieldSetter = field2setter.get(fieldMeta.getName());

        if (fieldSetter != null) {
          fieldSetter.set(object, cell, fieldMeta);
        } else {

          defaultSetter.set(object, cell, fieldMeta);
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