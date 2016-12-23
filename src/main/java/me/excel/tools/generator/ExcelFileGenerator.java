package me.excel.tools.generator;

import me.excel.tools.ExcelConstants;
import me.excel.tools.exporter.ExcelFileExporter;
import me.excel.tools.exporter.UserFileExporter;
import me.excel.tools.extractor.FieldValueExtractor;
import me.excel.tools.extractor.DefaultValueExtractor;
import me.excel.tools.model.excel.*;
import me.excel.tools.prompter.FieldPrompter;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileGenerator implements UserFileGenerator {

  private List data = new ArrayList<>();

  private List<String> fields = new ArrayList<>();

  private List<String> titles = new ArrayList<>();

  private DefaultValueExtractor defaultValueExtractor = new DefaultValueExtractor();

  private List<FieldValueExtractor> fieldValueExtractors = new ArrayList<>();

  private List<FieldPrompter> fieldPrompters = new ArrayList<>();

  @Override
  public void setTitles(String... titles) {
    if (titles == null) {
      throw new IllegalArgumentException("title is null");
    }
    Collections.addAll(this.titles, titles);
  }

  @Override
  public void setFields(String... fields) {
    if (fields == null) {
      throw new IllegalArgumentException("field is null");
    }
    Collections.addAll(this.fields, fields);
  }

  @Override
  public void addValueExtractors(FieldValueExtractor... fieldValueExtractors) {
    if (fieldValueExtractors == null) {
      return;
    }
    Collections.addAll(this.fieldValueExtractors, fieldValueExtractors);
  }

  @Override
  public void addCellPrompters(FieldPrompter... fieldPrompters) {
    if (fieldPrompters == null) {
      return;
    }
    Collections.addAll(this.fieldPrompters, fieldPrompters);
  }

  @Override
  public void setData(List data) {
    this.data = data;
  }

  @Override
  public void generate(File excel) throws IOException {
    generate(new FileOutputStream(excel));
  }

  @Override
  public void generate(OutputStream outputStream) throws IOException {
    generate(outputStream, true, true, true);
  }

  @Override
  public void generate(File excel, boolean createTitles, boolean createFields, boolean createPrompts) throws IOException {
    generate(new FileOutputStream(excel), createTitles, createFields, createPrompts);
  }

  @Override
  public void generate(OutputStream outputStream, boolean createTitles, boolean createFields, boolean createPrompts) throws IOException {

    ExcelWorkbook excelWorkbook = createWorkbook(createTitles, createFields, createPrompts);
    if (excelWorkbook == null) {
      throw new IllegalArgumentException("workbook is null");
    }

    UserFileExporter excelFileExporter = new ExcelFileExporter(excelWorkbook);
    excelFileExporter.export(outputStream);
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

    List<String> prompts = fieldPrompters.stream().filter(fieldPrompter -> fieldPrompter.matches(field)).map(FieldPrompter::getPrompt).collect(Collectors.toList());
    return prompts.isEmpty() ? ExcelConstants.EMPTY_VALUE : StringUtils.join(prompts, ExcelConstants.COMMA_SEPARATOR);
  }

  private String getFieldValue(Object data, String field) {

    for (FieldValueExtractor fieldValueExtractor : fieldValueExtractors) {
      if (fieldValueExtractor.matches(field)) {
        return fieldValueExtractor.getStringValue(data);
      }
    }

    return defaultValueExtractor.getStringValue(data, field);
  }
}
