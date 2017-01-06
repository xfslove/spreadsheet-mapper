package spreadsheet.mapper.o2w.composer;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.HeaderMeta;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.o2w.extractor.BeanUtilsValueExtractor;
import spreadsheet.mapper.o2w.extractor.FieldValueExtractor;
import spreadsheet.mapper.o2w.extractor.ValueExtractor;
import spreadsheet.mapper.model.core.*;

import java.util.*;

/**
 * Created by hanwen on 15-12-16.
 */
public class DefaultSheetComposeHelper<T> implements SheetComposeHelper<T> {

  private SheetMeta sheetMeta;

  private List<T> data = new ArrayList<>();

  private Map<String, FieldValueExtractor<T>> key2fieldValueExtractor = new LinkedHashMap<>();

  private ValueExtractor<T> defaultValueExtractor = new BeanUtilsValueExtractor<>();

  @Override
  @SuppressWarnings("unchecked")
  public SheetComposeHelper<T> fieldValueExtractor(FieldValueExtractor<T>... fieldValueExtractors) {
    if (fieldValueExtractors == null) {
      return this;
    }
    for (FieldValueExtractor<T> extractor : fieldValueExtractors) {
      key2fieldValueExtractor.put(extractor.getMatchField(), extractor);
    }
    return this;
  }

  @Override
  public SheetComposeHelper<T> sheetMeta(SheetMeta sheetMeta) {
    this.sheetMeta = sheetMeta;
    return this;
  }

  @Override
  public SheetComposeHelper<T> data(List<T> data) {
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
      createDataCells(row, object, sheetMeta);
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

  private void createDataCells(Row row, T object, SheetMeta sheetMeta) {

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

      String value = getFieldStringValue(object, fieldMeta);
      cell = new CellBean(value);
      row.addCell(cell);
    }
  }

  private String getFieldStringValue(T object, FieldMeta fieldMeta) {
    FieldValueExtractor<T> extractor = key2fieldValueExtractor.get(fieldMeta.getName());

    if (extractor != null) {
      return extractor.getStringValue(object, fieldMeta);
    }

    return defaultValueExtractor.getStringValue(object, fieldMeta);
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
