package excel.engine.o2w.composer;

import excel.engine.model.core.*;
import excel.engine.model.meta.FieldMeta;
import excel.engine.model.meta.HeaderMeta;
import excel.engine.model.meta.SheetMeta;
import excel.engine.o2w.extractor.BeanUtilValueExtractor;
import excel.engine.o2w.extractor.FieldValueExtractor;
import excel.engine.o2w.extractor.ValueExtractor;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by hanwen on 15-12-16.
 */
public class DefaultSheetComposer implements SheetComposer {

  private SheetMeta sheetMeta;

  private List<Object> data = new ArrayList<>();

  private Map<String, FieldValueExtractor> key2fieldValueExtractor = new HashMap<>();

  private ValueExtractor defaultValueExtractor = new BeanUtilValueExtractor();

  @Override
  public SheetComposer fieldValueExtractor(FieldValueExtractor... fieldValueExtractors) {
    if (fieldValueExtractors == null) {
      return this;
    }
    for (FieldValueExtractor extractor : fieldValueExtractors) {
      key2fieldValueExtractor.put(extractor.getMatchField(), extractor);
    }
    return this;
  }

  @Override
  public SheetComposer sheetMeta(SheetMeta sheetMeta) {
    this.sheetMeta = sheetMeta;
    return this;
  }

  @Override
  public SheetComposer data(List<Object> data) {
    this.data = data;
    return this;
  }

  @Override
  public Sheet compose() {
    Sheet sheet = createSheet(sheetMeta.getName());

    int lastRowNum = sheetMeta.getDataStartRowIndex() + data.size() - 1;

    for (int j = 1; j <= lastRowNum; j++) {

      Row row = createRow(j);
      sheet.addRow(row);

      if (row.getIndex() < sheetMeta.getDataStartRowIndex()) {

        createHeaderIfNecessary(row, sheetMeta);
      } else {

        createDataRowCells(row, data.get(j - 1), sheetMeta);
      }
    }

    return sheet;
  }

  private Sheet createSheet(String sheetName) {
    if (StringUtils.isBlank(sheetName)) {
      return new SheetBean();
    }
    return new SheetBean(sheetName);
  }

  private Row createRow(int rowIndex) {
    return new RowBean(rowIndex);
  }

  private void createHeaderIfNecessary(Row row, SheetMeta sheetMeta) {
    int rowIndex = row.getIndex();

    List<FieldMeta> fieldMetas = sheetMeta.getFieldMetas();

    for (FieldMeta fieldMeta : fieldMetas) {

      HeaderMeta headerMeta = fieldMeta.getHeaderMeta(rowIndex);

      int columnIndex = fieldMeta.getColumnIndex();

      CellBean headerCell;
      if (headerMeta == null) {

        headerCell = CellBean.EMPTY_CELL(rowIndex, columnIndex);
      } else {

        headerCell = new CellBean(rowIndex, columnIndex, headerMeta.getValue());
      }

      row.addCell(headerCell);
    }

  }

  private void createDataRowCells(Row row, Object object, SheetMeta sheetMeta) {

    List<FieldMeta> fieldMetas = sheetMeta.getFieldMetas();

    for (FieldMeta fieldMeta : fieldMetas) {

      String value = getFieldStringValue(object, fieldMeta);
      CellBean dataCell = new CellBean(row.getIndex(), fieldMeta.getColumnIndex(), value);

      row.addCell(dataCell);

    }
  }

  private String getFieldStringValue(Object object, FieldMeta fieldMeta) {
    if (MapUtils.isNotEmpty(key2fieldValueExtractor)) {

      FieldValueExtractor extractor = key2fieldValueExtractor.get(fieldMeta.getName());
      if (extractor != null) {
        return extractor.getStringValue(object, fieldMeta);
      }
    }

    return defaultValueExtractor.getStringValue(object, fieldMeta);
  }
}
