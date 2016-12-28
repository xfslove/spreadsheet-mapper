package me.excel.tools.generator;

import me.excel.tools.extractor.DefaultValueExtractor;
import me.excel.tools.extractor.FieldValueExtractor;
import me.excel.tools.helper.WorkbookToExcelHelper;
import me.excel.tools.model.excel.*;
import me.excel.tools.model.template.HeaderDetail;
import me.excel.tools.model.template.SheetHeader;
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
public class DefaultExcelGenerator implements ExcelGenerator {

  private DefaultValueExtractor defaultValueExtractor = new DefaultValueExtractor();

  private Map<Integer, Map<String, FieldValueExtractor>> key2fieldValueExtractor = new HashMap<>();

  private Map<Integer, SheetContext> contextMap = new HashMap<>();

  @Override
  public void addValueExtractors(FieldValueExtractor... fieldValueExtractors) {
    if (fieldValueExtractors == null) {
      return;
    }
    for (FieldValueExtractor extractor : fieldValueExtractors) {
      int sheetIndex = extractor.getSheetIndex();

      if (!key2fieldValueExtractor.containsKey(sheetIndex)) {
        key2fieldValueExtractor.put(sheetIndex, new HashMap<>());
      }

      key2fieldValueExtractor.get(sheetIndex).put(extractor.getMatchField(), extractor);
    }
  }

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
  public void write(File excel) throws IOException {
    write(new FileOutputStream(excel));
  }

  @Override
  public void write(OutputStream outputStream) throws IOException {

    if (MapUtils.isEmpty(contextMap)) {
      throw new IllegalArgumentException("context is null");
    }

    Workbook workbook = createWorkbook();
    WorkbookToExcelHelper.write(outputStream, workbook);
  }

  private Workbook createWorkbook() {

    WorkbookBean workbook = new WorkbookBean();

    for (int i = 1; i <= contextMap.size(); i++) {

      SheetContext context = contextMap.get(i);

      Sheet sheet = createSheet(context.getSheetName());
      workbook.addSheet(sheet);

      if (context.getHeader() != null) {
        sheet.setHeader(context.getHeader());
      }

      List<HeaderDetail> headerDetails = context.getHeaderDetails();

      List<Object> data = context.getData();
      int lastRowNum = sheet.getHeader().getDataStartRowIndex() + data.size() - 1;

      for (int j = 1; j <= lastRowNum; j++) {

        Row row = createRow(j);
        sheet.addRow(row);

        createHeaderIfNecessary(row, sheet.getHeader(), headerDetails);
        createDataRowCells(row, data.get(j - 1), headerDetails, sheet.getIndex());
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

  private void createHeaderIfNecessary(Row row, SheetHeader header, List<HeaderDetail> details) {
    int rowIndex = row.getIndex();

    if (rowIndex >= header.getDataStartRowIndex()) {
      return;
    }

    if (header.isHasTitle() && rowIndex == header.getTitleRowIndex()) {

      createTitleRowCells(row, details);
    } else if (header.isHasField() && rowIndex == header.getFieldRowIndex()) {

      createFieldRowCells(row, details);
    } else if (header.isHasPrompt() && rowIndex == header.getPromptRowIndex()) {

      createPromptRowCells(row, details);
    }
  }

  private void createDataRowCells(Row row, Object object, List<HeaderDetail> details, int sheetIndex) {
    for (int i = 1; i <= details.size(); i++) {

      HeaderDetail detail = details.get(i - 1);
      String value = getFieldValue(object, detail.getField(), sheetIndex);
      CellBean dataCell = new CellBean(row.getIndex(), i, value);
      dataCell.setField(detail.getField());

      row.addCell(dataCell);
    }

  }

  private void createTitleRowCells(Row row, List<HeaderDetail> details) {
    for (int i = 1; i <= details.size(); i++) {

      HeaderDetail detail = details.get(i - 1);
      CellBean titleCell = new CellBean(row.getIndex(), i, detail.getTitle());
      titleCell.setField(detail.getField());

      row.addCell(titleCell);
    }

  }


  private void createFieldRowCells(Row row, List<HeaderDetail> details) {
    for (int i = 1; i <= details.size(); i++) {

      HeaderDetail detail = details.get(i - 1);
      CellBean fieldCell = new CellBean(row.getIndex(), i, detail.getField());
      fieldCell.setField(detail.getField());

      row.addCell(fieldCell);
    }
  }

  private void createPromptRowCells(Row row, List<HeaderDetail> details) {
    for (int i = 1; i <= details.size(); i++) {

      HeaderDetail detail = details.get(i - 1);
      CellBean promptCell = new CellBean(row.getIndex(), i, detail.getPrompt());
      promptCell.setField(detail.getField());

      row.addCell(promptCell);
    }
  }

  private String getFieldValue(Object object, String field, int sheetIndex) {

    Map<String, FieldValueExtractor> valueExtractorOfSheet = key2fieldValueExtractor.get(sheetIndex);
    if (MapUtils.isEmpty(valueExtractorOfSheet)) {

      return defaultValueExtractor.getStringValue(object, field);
    }

    FieldValueExtractor extractor = valueExtractorOfSheet.get(field);

    if (extractor != null) {
      return extractor.getStringValue(object);
    }

    return defaultValueExtractor.getStringValue(object, field);
  }
}
