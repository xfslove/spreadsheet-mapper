package excel.engine.exporter.composer;

import excel.engine.exporter.extractor.DefaultValueExtractor;
import excel.engine.exporter.extractor.FieldValueExtractor;
import excel.engine.model.excel.*;
import excel.engine.model.ext.HeaderMeta;
import excel.engine.model.ext.SheetContext;
import excel.engine.model.ext.SheetHeader;
import excel.engine.util.ExcelWriteHelper;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by hanwen on 15-12-16.
 */
public class DefaultExcelComposerEngine implements ExcelComposerEngine {

  private Map<Integer, Map<String, FieldValueExtractor>> key2fieldValueExtractor = new HashMap<>();

  private Map<Integer, SheetContext> contextMap = new HashMap<>();

  @Override
  public void addSheetContext(SheetContext... sheetContexts) {
    if (sheetContexts == null) {
      return;
    }

    for (SheetContext sheetContext : sheetContexts) {
      contextMap.put(sheetContext.getSheetIndex(), sheetContext);
    }
  }

  @Override
  public void addFieldValueExtractor(FieldValueExtractor... fieldValueExtractors) {
    if (fieldValueExtractors == null) {
      return;
    }
    for (FieldValueExtractor extractor : fieldValueExtractors) {
      int sheetIndex = extractor.getSheetIndex();

      if (!key2fieldValueExtractor.containsKey(sheetIndex)) {
        key2fieldValueExtractor.put(sheetIndex, new HashMap<String, FieldValueExtractor>());
      }

      key2fieldValueExtractor.get(sheetIndex).put(extractor.getMatchField(), extractor);
    }
  }

  @Override
  public void write(File excel) throws IOException {
    write(new FileOutputStream(excel));
  }

  @Override
  public void write(OutputStream outputStream) throws IOException {

    if (MapUtils.isEmpty(contextMap)) {
      throw new IllegalArgumentException("context is null");
    }

    Workbook workbook = createWorkbook();
    ExcelWriteHelper.write(outputStream, workbook);
  }

  private Workbook createWorkbook() {

    WorkbookBean workbook = new WorkbookBean();

    for (int i = 1; i <= contextMap.size(); i++) {

      SheetContext context = contextMap.get(i);

      Sheet sheet = createSheet(context.getSheetName());
      workbook.addSheet(sheet);

      sheet.setTemplate(context.ofTemplate());

      List<Object> data = context.getData();
      int lastRowNum = sheet.getTemplate().getDataStartRowIndex() + data.size() - 1;

      for (int j = 1; j <= lastRowNum; j++) {

        Row row = createRow(j);
        sheet.addRow(row);

        createHeaderIfNecessary(row, context);
        createDataRowCells(row, data.get(j - 1), context);
      }

    }

    return workbook;
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

  private void createHeaderIfNecessary(Row row, SheetContext context) {
    int rowIndex = row.getIndex();

    if (rowIndex >= context.getDataStartRowIndex()) {
      return;
    }

    List<String> fields = context.getFields();
    for (SheetHeader sheetHeader : context.getSheetHeaders()) {
      HeaderMeta headerMeta = sheetHeader.getHeaderMeta();

      if (headerMeta.getRowIndex() == rowIndex) {

        for (int i = 1; i <= fields.size(); i++) {
          String field = fields.get(i - 1);
          CellBean headerCell = new CellBean(rowIndex, i, sheetHeader.getValue(field));
          headerCell.setField(field);

          row.addCell(headerCell);
        }
      }
    }
  }

  private void createDataRowCells(Row row, Object object, SheetContext context) {
    List<String> fields = context.getFields();

    for (int i = 1; i <= fields.size(); i++) {

      String field = fields.get(i - 1);
      String value = getFieldStringValue(object, field, context.getSheetIndex());
      CellBean dataCell = new CellBean(row.getIndex(), i, value);
      dataCell.setField(field);

      row.addCell(dataCell);
    }

  }

  private String getFieldStringValue(Object object, String field, int sheetIndex) {
    Map<String, FieldValueExtractor> valueExtractorOfSheet = key2fieldValueExtractor.get(sheetIndex);

    if (MapUtils.isNotEmpty(valueExtractorOfSheet)) {

      FieldValueExtractor extractor = valueExtractorOfSheet.get(field);
      if (extractor != null) {
        return extractor.getStringValue(object);
      }
    }

    return DefaultValueExtractor.getStringValue(object, field);
  }
}
