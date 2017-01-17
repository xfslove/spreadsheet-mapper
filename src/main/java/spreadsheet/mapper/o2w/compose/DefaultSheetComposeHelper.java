package spreadsheet.mapper.o2w.compose;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.model.core.*;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.HeaderMeta;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.o2w.compose.converter.BeanUtilsConverter;
import spreadsheet.mapper.o2w.compose.converter.Converter;
import spreadsheet.mapper.o2w.compose.converter.FieldConverter;

import java.util.*;

/**
 * Created by hanwen on 15-12-16.
 */
public class DefaultSheetComposeHelper<T> implements SheetComposeHelper<T> {

  private SheetMeta sheetMeta;

  private List<T> data = new ArrayList<>();

  private Map<String, FieldConverter<T>> field2converter = new LinkedHashMap<>();

  private Converter<T> defaultConverter = new BeanUtilsConverter<>();

  @Override
  public SheetComposeHelper<T> addFieldConverter(FieldConverter<T> fieldConverter) {
    if (fieldConverter == null) {
      throw new WorkbookComposeException("field converter can not be null");
    }

    field2converter.put(fieldConverter.getMatchField(), fieldConverter);
    return this;
  }

  @Override
  public SheetComposeHelper<T> setSheetMeta(SheetMeta sheetMeta) {
    if (sheetMeta == null) {
      throw new WorkbookComposeException("sheet meta can not be null");
    }
    this.sheetMeta = sheetMeta;
    return this;
  }

  @Override
  public SheetComposeHelper<T> setData(List<T> data) {
    if (data == null) {
      throw new WorkbookComposeException("data can not be null");
    }
    this.data = data;
    return this;
  }

  @Override
  public Sheet compose() {
    if (sheetMeta == null) {
      throw new WorkbookComposeException("set sheet meta first");
    }

    Sheet sheet = createSheet(sheetMeta);

    int dataStartRowIndex = sheetMeta.getDataStartRowIndex();
    for (int i = 1; i < dataStartRowIndex; i++) {

      Row row = createRow();
      sheet.addRow(row);
      createHeaderCellsIfNecessary(row, sheetMeta);
    }

    if (CollectionUtils.isEmpty(data)) {
      return sheet;
    }

    for (T object : data) {
      Row row = createRow();
      sheet.addRow(row);
      createDataCells(object, row, sheetMeta);
    }

    return sheet;
  }

  private Sheet createSheet(SheetMeta sheetMeta) {
    String sheetName = sheetMeta.getSheetName();

    if (StringUtils.isBlank(sheetName)) {
      return new SheetBean();
    }
    return new SheetBean(sheetName);
  }

  private Row createRow() {
    return new RowBean();
  }

  private void createHeaderCellsIfNecessary(Row row, SheetMeta sheetMeta) {
    List<FieldMeta> fieldMetas = sheetMeta.getFieldMetas();

    int lastColumnNum = getLastColumnNum(fieldMetas);
    Map<Integer, FieldMeta> columnIndex2fieldMeta = buildFieldMetaMap(fieldMetas);

    for (int i = 1; i <= lastColumnNum; i++) {
      Cell cell;
      FieldMeta fieldMeta = columnIndex2fieldMeta.get(i);

      if (fieldMeta == null) {

        cell = new CellBean();
        row.addCell(cell);
        continue;
      }

      HeaderMeta headerMeta = fieldMeta.getHeaderMeta(row.getIndex());
      if (headerMeta == null) {

        cell = new CellBean();
        row.addCell(cell);
        continue;
      }

      cell = new CellBean(headerMeta.getValue());
      row.addCell(cell);
    }

  }

  private void createDataCells(T object, Row row, SheetMeta sheetMeta) {

    List<FieldMeta> fieldMetas = sheetMeta.getFieldMetas();
    int lastColumnNum = getLastColumnNum(fieldMetas);
    Map<Integer, FieldMeta> columnIndex2fieldMeta = buildFieldMetaMap(fieldMetas);

    for (int i = 1; i <= lastColumnNum; i++) {
      Cell cell = new CellBean();
      FieldMeta fieldMeta = columnIndex2fieldMeta.get(i);

      if (fieldMeta == null) {
        row.addCell(cell);
        continue;
      }

      // use default converter first
      cell = new CellBean(defaultConverter.getValue(object, cell, fieldMeta));

      FieldConverter<T> converter = field2converter.get(fieldMeta.getName());

      if (converter != null) {
        cell = new CellBean(converter.getValue(object, cell, fieldMeta));
      }

      row.addCell(cell);
    }
  }

  private int getLastColumnNum(List<FieldMeta> fieldMetas) {
    if (CollectionUtils.isEmpty(fieldMetas)) {
      return 0;
    }
    FieldMeta lastFieldMeta = fieldMetas.get(fieldMetas.size() - 1);
    return lastFieldMeta.getColumnIndex();
  }

  private Map<Integer, FieldMeta> buildFieldMetaMap(List<FieldMeta> fieldMetas) {
    Map<Integer, FieldMeta> columnIndex2fieldMeta = new HashMap<>();
    for (FieldMeta fieldMeta : fieldMetas) {
      columnIndex2fieldMeta.put(fieldMeta.getColumnIndex(), fieldMeta);
    }
    return columnIndex2fieldMeta;
  }
}
