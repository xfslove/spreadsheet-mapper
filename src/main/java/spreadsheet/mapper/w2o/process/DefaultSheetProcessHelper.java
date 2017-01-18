package spreadsheet.mapper.w2o.process;

import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.w2o.process.listener.*;
import spreadsheet.mapper.w2o.process.setter.BeanUtilsSetter;
import spreadsheet.mapper.w2o.process.setter.FieldSetter;
import spreadsheet.mapper.w2o.process.setter.Setter;

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
  public SheetProcessHelper<T> addFieldSetter(FieldSetter<T> fieldSetter) {
    if (fieldSetter == null) {
      throw new WorkbookProcessException("field setter can not be null");
    }

    field2setter.put(fieldSetter.getMatchField(), fieldSetter);
    return this;
  }

  @Override
  public SheetProcessHelper<T> setObjectFactory(ObjectFactory<T> objectFactory) {
    if (objectFactory == null) {
      throw new WorkbookProcessException("object factory can not be null");
    }

    this.objectFactory = objectFactory;
    return this;
  }

  @Override
  public SheetProcessHelper<T> setSheetProcessorListener(SheetProcessListener<T> sheetProcessListener) {
    if (sheetProcessListener == null) {
      throw new WorkbookProcessException("sheet process listener can not be null");
    }

    this.sheetProcessListener = sheetProcessListener;
    return this;
  }

  @Override
  public SheetProcessHelper<T> setRowProcessorListener(RowProcessListener<T> rowProcessListener) {
    if (rowProcessListener == null) {
      throw new WorkbookProcessException("row process listener can not be null");
    }

    this.rowProcessListener = rowProcessListener;
    return this;
  }

  @Override
  public SheetProcessHelper<T> setCellProcessorListener(CellProcessListener<T> cellProcessListener) {
    if (cellProcessListener == null) {
      throw new WorkbookProcessException("cell process listener can not be null");
    }

    this.cellProcessListener = cellProcessListener;
    return this;
  }

  @Override
  public SheetProcessHelper<T> setSheet(Sheet sheet) {
    if (sheet == null) {
      throw new WorkbookProcessException("sheet can not be null");
    }

    this.sheet = sheet;
    return this;
  }

  @Override
  public SheetProcessHelper<T> setSheetMeta(SheetMeta sheetMeta) {
    if (sheetMeta == null) {
      throw new WorkbookProcessException("sheet meta can not be null");
    }

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

      T object = objectFactory.create(row, sheetMeta);

      rowProcessListener.before(object, row, sheetMeta);

      for (Cell cell : row.getCells()) {

        FieldMeta fieldMeta = columnIndex2fieldMeta.get(cell.getIndex());

        if (fieldMeta == null) {
          // if missing field meta skip the cell(same column index with field meta)
          continue;
        }

        cellProcessListener.before(object, cell, fieldMeta);

        // use default setter first
        defaultSetter.set(object, cell, fieldMeta);

        FieldSetter<T> fieldSetter = field2setter.get(fieldMeta.getName());

        if (fieldSetter != null) {
          fieldSetter.set(object, cell, fieldMeta);
        }

        cellProcessListener.after(object, cell, fieldMeta);

      }

      rowProcessListener.after(object, row, sheetMeta);

      oneSheetObjects.add(object);
    }

    sheetProcessListener.after(oneSheetObjects, sheet, sheetMeta);

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