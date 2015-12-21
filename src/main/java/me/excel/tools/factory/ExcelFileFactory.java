package me.excel.tools.factory;

import me.excel.tools.exporter.ExcelFileExporter;
import me.excel.tools.model.excel.*;
import me.excel.tools.utils.DefaultFieldValueExtractor;
import me.excel.tools.utils.FieldValueExtractor;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileFactory implements UserFileFactory {

  protected List datas = new ArrayList<>();

  protected List<String> fields = new ArrayList<>();

  protected ExcelFileExporter excelFileExporter;

  protected FieldValueExtractor fieldValueExtractor;

  protected ImportTemplate importTemplate;

  public ExcelFileFactory(ImportTemplate importTemplate) {
    this.importTemplate = importTemplate;
    this.fieldValueExtractor = new DefaultFieldValueExtractor();
  }

  public void setFields(String... fields) {
    if (fields == null) {
      throw new IllegalArgumentException("field is null");
    }
    for (String field : fields) {
      this.fields.add(field);
    }
  }

  public void setDatas(List datas) {
    this.datas = datas;
  }

  public void generate(File excel) throws IOException {
    ExcelWorkbook excelWorkbook = createWorkbook();
    if (excelWorkbook == null) {
      throw new IllegalArgumentException("workbook is null");
    }

    excelWorkbook.getSheet(0).getDataRows().forEach(row -> {

      if (row.sizeOfCells() != fields.size()) {
        throw new IllegalArgumentException("fields mistake at row"+row.getRowNum());
      }

    });

    excelFileExporter = new ExcelFileExporter(excelWorkbook);
    try(OutputStream outputStream = new FileOutputStream(excel)) {
      excelFileExporter.export(outputStream);
    }
  }

  private ExcelWorkbook createWorkbook() {
    ExcelWorkbookBean workbook = new ExcelWorkbookBean();

    ExcelSheetBean sheet = new ExcelSheetBean();
    sheet.setWorkbook(workbook);
    workbook.addSheet(sheet);

    int rowIndex = 1;
    createTitleRow(sheet, rowIndex);
    rowIndex++;
    createFieldRow(sheet, rowIndex);
    rowIndex++;
    createPromptRow(sheet, rowIndex);
    rowIndex++;
    for (Object data : datas) {
      createDataRow(sheet, data, rowIndex);
      rowIndex++;
    }

    return workbook;
  }

  private void createTitleRow(ExcelSheetBean sheet, int rowIndex) {
    ExcelRowBean row = new ExcelRowBean(rowIndex);
    sheet.addRow(row);

    for (int i = 0; i < fields.size(); i++) {
      String field = fields.get(i);
      ExcelCellBean cell =  new ExcelCellBean(rowIndex, i + 1, null, getI18nValue(field));
      row.addCell(cell);
    }
  }

  private void createFieldRow(ExcelSheetBean sheet, int rowIndex) {
    ExcelRowBean row = new ExcelRowBean(rowIndex);
    sheet.addRow(row);

    for (int i = 0; i < fields.size(); i++) {
      String field = fields.get(i);
      ExcelCellBean cell =  new ExcelCellBean(rowIndex, i + 1, null, field);
      row.addCell(cell);
    }
  }

  private void createPromptRow(ExcelSheetBean sheet, int rowIndex) {
    ExcelRowBean row = new ExcelRowBean(rowIndex);
    sheet.addRow(row);

    for (int i = 0; i < fields.size(); i++) {
      String field = fields.get(i);
      ExcelCellBean cell =  new ExcelCellBean(rowIndex, i + 1, null, getPrompts(field));
      row.addCell(cell);
    }
  }

  private void createDataRow(ExcelSheetBean sheet, Object data, int rowIndex) {
    ExcelRowBean row = new ExcelRowBean(rowIndex);
    sheet.addRow(row);

    for (int i = 0; i < fields.size(); i++) {
      String field = fields.get(i);
      String fieldValue = getFieldValue(data, field);
      ExcelCellBean cell = new ExcelCellBean(rowIndex, i + 1, field, fieldValue);
      row.addCell(cell);
    }
  }

  private String getI18nValue(String field) {
    // TODO get i18n title
    return field;
  }

  private String getPrompts(String field) {
    List<String> prompts = importTemplate.getValidators().stream()
        .filter(validator -> validator.matches(field))
        .map(validator -> validator.getPrompt()).collect(Collectors.toList());
    return prompts.isEmpty() ? "" : StringUtils.join(prompts, ",");
  }

  private String getFieldValue(Object data, String field) {
    return fieldValueExtractor.getStringValue(data, field);
  }
}
