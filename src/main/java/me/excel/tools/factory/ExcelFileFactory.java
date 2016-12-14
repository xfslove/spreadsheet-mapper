package me.excel.tools.factory;

import me.excel.tools.exporter.ExcelFileExporter;
import me.excel.tools.exporter.UserFileExporter;
import me.excel.tools.extractor.CellValueExtractor;
import me.excel.tools.extractor.DefaultValueExtractor;
import me.excel.tools.model.excel.*;
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

  protected List data = new ArrayList<>();

  protected List<String> fields = new ArrayList<>();

  protected List<String> titles = new ArrayList<>();

  protected DefaultValueExtractor defaultValueExtractor = new DefaultValueExtractor();

  protected List<CellValueExtractor> cellValueExtractors = new ArrayList<>();

  protected FileTemplate fileTemplate;

  protected File file;

  public ExcelFileFactory(FileTemplate fileTemplate, File file) {
    this.fileTemplate = fileTemplate;
    this.file = file;
  }

  @Override
  public void setTitles(String... titles) {
    if (titles == null) {
      throw new IllegalArgumentException("title is null");
    }
    for (String title : titles) {
      this.titles.add(title);
    }
  }

  @Override
  public void setFields(String... fields) {
    if (fields == null) {
      throw new IllegalArgumentException("field is null");
    }
    for (String field : fields) {
      this.fields.add(field);
    }
  }

  @Override
  public void addValueExtractors(CellValueExtractor... cellValueExtractors) {
    if (cellValueExtractors == null) {
      return;
    }
    for (CellValueExtractor cellValueExtractor : cellValueExtractors) {
      this.cellValueExtractors.add(cellValueExtractor);
    }
  }

  @Override
  public void setData(List data) {
    this.data = data;
  }

  @Override
  public void generate() throws IOException {
    generate(true, true, true);
  }

  @Override
  public void generate(boolean createTitles, boolean createFields, boolean createPrompts) throws IOException {

    ExcelWorkbook excelWorkbook = createWorkbook(createTitles, createFields, createPrompts);
    if (excelWorkbook == null) {
      throw new IllegalArgumentException("workbook is null");
    }

    excelWorkbook.getSheet(0).getDataRows().forEach(row -> {

      if (row.sizeOfCells() != fields.size()) {
        throw new IllegalArgumentException("fields mistake at row" + row.getRowNum());
      }

    });

    UserFileExporter excelFileExporter = new ExcelFileExporter(excelWorkbook);
    try (OutputStream outputStream = new FileOutputStream(file)) {
      excelFileExporter.export(outputStream);
    }
  }

  private ExcelWorkbook createWorkbook(boolean createTitles, boolean createFields, boolean createPrompts) {
    ExcelWorkbookBean workbook = new ExcelWorkbookBean();

    ExcelSheetBean sheet = new ExcelSheetBean();
    sheet.setWorkbook(workbook);
    workbook.addSheet(sheet);

    int rowIndex = 1;
    if (createTitles) {
      createTitleRow(sheet, rowIndex);
      rowIndex++;
    }
    if (createFields) {
      createFieldRow(sheet, rowIndex);
      rowIndex++;
    }

    if (createPrompts) {
      createPromptRow(sheet, rowIndex);
      rowIndex++;
    }

    for (Object data : this.data) {
      createDataRow(sheet, data, rowIndex);
      rowIndex++;
    }

    return workbook;
  }

  private ExcelRow createTitleRow(ExcelSheetBean sheet, int rowIndex) {
    ExcelRowBean row = new ExcelRowBean(rowIndex);
    sheet.addRow(row);

    for (int i = 0; i < titles.size(); i++) {
      ExcelCellBean cell = new ExcelCellBean(rowIndex, i + 1, null, titles.get(i));
      row.addCell(cell);
    }
    return row;
  }

  private ExcelRow createFieldRow(ExcelSheetBean sheet, int rowIndex) {
    ExcelRowBean row = new ExcelRowBean(rowIndex);
    sheet.addRow(row);

    for (int i = 0; i < fields.size(); i++) {
      ExcelCellBean cell = new ExcelCellBean(rowIndex, i + 1, null, fields.get(i));
      row.addCell(cell);
    }
    return row;
  }

  private ExcelRow createPromptRow(ExcelSheetBean sheet, int rowIndex) {
    ExcelRowBean row = new ExcelRowBean(rowIndex);
    sheet.addRow(row);

    for (int i = 0; i < fields.size(); i++) {
      String field = fields.get(i);
      ExcelCellBean cell = new ExcelCellBean(rowIndex, i + 1, field, getPrompts(field));
      row.addCell(cell);
    }
    return row;
  }

  private ExcelRow createDataRow(ExcelSheetBean sheet, Object data, int rowIndex) {
    ExcelRowBean row = new ExcelRowBean(rowIndex);
    sheet.addRow(row);

    for (int i = 0; i < fields.size(); i++) {
      String field = fields.get(i);
      String fieldValue = getFieldValue(data, field);
      ExcelCellBean cell = new ExcelCellBean(rowIndex, i + 1, field, fieldValue);
      row.addCell(cell);
    }
    return row;
  }

  private String getPrompts(String field) {

    List<String> promptOfRowValidator = new ArrayList<>();

    fileTemplate.getRowValidators().forEach(rowValidator -> promptOfRowValidator.addAll(rowValidator.getMessageOnFields().stream()
        .filter(f -> f.equals(field) && rowValidator.getPrompt() != null).map(f -> rowValidator.getPrompt()).collect(Collectors.toList())));

    String promptOfRowValidatorString = promptOfRowValidator.isEmpty() ? "" : StringUtils.join(promptOfRowValidator, ",");

    List<String> promptOfCellValidator = fileTemplate.getCellValidators().stream()
        .filter(validator -> validator.matches(field)).filter(validator -> validator.getPrompt() != null)
        .map(validator -> validator.getPrompt()).collect(Collectors.toList());

    String promptOfCellValidatorString = promptOfCellValidator.isEmpty() ? "" : StringUtils.join(promptOfCellValidator, ",");

    return promptOfCellValidatorString + promptOfRowValidatorString;
  }

  private String getFieldValue(Object data, String field) {

    for (CellValueExtractor cellValueExtractor : cellValueExtractors) {
      if (cellValueExtractor.matches(field)) {
        return cellValueExtractor.getStringValue(data);
      }
    }

    return defaultValueExtractor.getStringValue(data, field);
  }
}
