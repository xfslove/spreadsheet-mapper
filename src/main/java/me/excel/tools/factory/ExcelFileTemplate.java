package me.excel.tools.factory;

import me.excel.tools.exception.ExcelReadException;
import me.excel.tools.helper.ExcelToWorkbookHelper;
import me.excel.tools.model.excel.ExcelWorkbook;
import me.excel.tools.model.template.ExcelSheetHeaderInfo;
import me.excel.tools.processor.SheetToObjectsProcessor;
import me.excel.tools.validator.ExcelFileValidator;
import me.excel.tools.validator.UserFileValidator;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Set;

/**
 * excel template factory, only support single sheet importing, multi sheet not support.
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileTemplate implements UserFileTemplate {

  private UserFileValidator userFileValidator;

  private SheetToObjectsProcessor sheetToObjectsProcessor;

  private ExcelWorkbook excelWorkbook;

  public ExcelFileTemplate(File excel) throws IOException {
    this(new FileInputStream(excel));
  }

  public ExcelFileTemplate(InputStream inputStream) throws IOException {

    excelWorkbook = ExcelToWorkbookHelper.read(inputStream, ExcelSheetHeaderInfo.SINGLE_SHEET_DEFAULT);

    sheetToObjectsProcessor = new SheetToObjectsProcessor(excelWorkbook.getFirstSheet());

    userFileValidator = new ExcelFileValidator(excelWorkbook);
  }

  @Override
  public UserFileValidator getUserFileValidator() {
    return userFileValidator;
  }

  public SheetToObjectsProcessor getSheetToObjectsProcessor() {
    return sheetToObjectsProcessor;
  }

  @Override
  public Set<String> getDistinctValuesOfField(String field) {

    if (excelWorkbook == null) {
      throw new ExcelReadException("workbook is null");
    }

    return excelWorkbook.getFirstSheet().getDistinctValuesOfField(field);
  }

}