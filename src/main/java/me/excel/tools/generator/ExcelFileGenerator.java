package me.excel.tools.generator;

import me.excel.tools.extractor.DefaultValueExtractor;
import me.excel.tools.extractor.FieldValueExtractor;
import me.excel.tools.helper.WorkbookToExcelHelper;
import me.excel.tools.model.excel.*;
import me.excel.tools.model.template.HeaderDetail;
import me.excel.tools.model.template.SheetHeader;
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
public class ExcelFileGenerator implements UserFileGenerator {

  private DefaultValueExtractor defaultValueExtractor = new DefaultValueExtractor();

  private Map<String, FieldValueExtractor> key2fieldValueExtractor = new HashMap<>();

  @Override
  public void addValueExtractors(FieldValueExtractor... fieldValueExtractors) {
    if (fieldValueExtractors == null) {
      return;
    }
    for (FieldValueExtractor extractor : fieldValueExtractors) {
      key2fieldValueExtractor.put(extractor.getMatchField(), extractor);
    }
  }

  @Override
  public void generate(File excel, SheetContext... contexts) throws IOException {
    generate(new FileOutputStream(excel), contexts);
  }

  @Override
  public void generate(OutputStream outputStream, SheetContext... contexts) throws IOException {

    if (contexts == null) {
      throw new IllegalArgumentException("context is null");
    }

    Workbook workbook = createWorkbook(contexts);
    WorkbookToExcelHelper.write(outputStream, workbook);
  }

  private Workbook createWorkbook(SheetContext[] contexts) {

    Map<Integer, SheetContext> sheetIndex2context = buildContextMap(contexts);

    WorkbookBean workbook = new WorkbookBean();

    for (int i = 1; i <= sheetIndex2context.size(); i++) {

      SheetContext context = sheetIndex2context.get(i);

      Sheet sheet = createSheet(context.getSheetName());
      workbook.addSheet(sheet);

      if (context.getHeader() != null) {
        sheet.setHeader(context.getHeader());
      }

      List<HeaderDetail> headerDetails = context.getHeaderDetails();

      List data = context.getData();
      int lastRowNum = sheet.getHeader().getDataStartRowIndex() + data.size() - 1;

      for (int j = 1; j <= lastRowNum; j++) {

        Row row = createRow(j);
        sheet.addRow(row);

        createHeaderIfNecessary(row, sheet.getHeader(), headerDetails);
        createDataRowCells(row, data.get(j - 1), headerDetails);
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

    if (header.isHasTitle() && rowIndex == header.getTitleRowIndex()) {

      createTitleRowCells(row, details);
    } else if (header.isHasField() && rowIndex == header.getFieldRowIndex()) {

      createFieldRowCells(row, details);
    } else if (header.isHasPrompt() && rowIndex == header.getPromptRowIndex()) {

      createPromptRowCells(row, details);
    }
  }

  private void createDataRowCells(Row row, Object object, List<HeaderDetail> details) {
    for (int i = 1; i <= details.size(); i++) {

      HeaderDetail detail = details.get(i - 1);
      String value = getFieldValue(object, detail.getField());
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

  private Map<Integer, SheetContext> buildContextMap(SheetContext[] contexts) {
    Map<Integer, SheetContext> sheetIndex2context = new HashMap<>();

    for (SheetContext context : contexts) {
      sheetIndex2context.put(context.getSheetIndex(), context);
    }

    return sheetIndex2context;
  }

  private String getFieldValue(Object object, String field) {

    FieldValueExtractor extractor = key2fieldValueExtractor.get(field);

    if (extractor != null) {
      return extractor.getStringValue(object);
    }

    return defaultValueExtractor.getStringValue(object, field);
  }
}
