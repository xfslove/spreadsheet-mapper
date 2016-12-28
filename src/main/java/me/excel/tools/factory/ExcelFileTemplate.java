package me.excel.tools.factory;

import me.excel.tools.exception.ExcelReadException;
import me.excel.tools.helper.ExcelToWorkbookHelper;
import me.excel.tools.model.excel.Workbook;
import me.excel.tools.processor.DefaultObjectProcessor;
import me.excel.tools.processor.ObjectProcessor;
import me.excel.tools.validator.DefaultExcelValidator;
import me.excel.tools.validator.ExcelValidator;

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

  private ExcelValidator excelValidator;

  private ObjectProcessor objectProcessor;

  private Workbook workbook;

  public ExcelFileTemplate(File excel) throws IOException {
    this(new FileInputStream(excel));
  }

  public ExcelFileTemplate(InputStream inputStream) throws IOException {

    workbook = ExcelToWorkbookHelper.read(inputStream);

    objectProcessor = new DefaultObjectProcessor(workbook);

    excelValidator = new DefaultExcelValidator(workbook);
  }

  @Override
  public ExcelValidator getExcelValidator() {
    return excelValidator;
  }

  @Override
  public ObjectProcessor getObjectProcessor() {
    return objectProcessor;
  }

  @Override
  public Set<String> getDistinctValuesOfField(String field) {

    if (workbook == null) {
      throw new ExcelReadException("workbook is null");
    }

    return workbook.getFirstSheet().getDistinctValuesOfField(field);
  }

}