package me.excel.tools.factory;

import me.excel.tools.generator.ExcelFileGenerator;
import me.excel.tools.generator.UserFileGenerator;
import me.excel.tools.importer.ExcelFileImporter;
import me.excel.tools.importer.UserFileImporter;
import me.excel.tools.model.excel.ExcelWorkbook;
import me.excel.tools.transfer.ExcelFileTransfer;
import me.excel.tools.transfer.ExcelFileTransferImpl;
import me.excel.tools.validator.ExcelFileValidator;
import me.excel.tools.validator.UserFileValidator;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.Set;

/**
 * excel 模板工厂, 只支持单sheet的导入，多sheet不支持
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileTemplate implements UserFileTemplate {

  private UserFileGenerator userFileGenerator;

  private UserFileValidator userFileValidator;

  private UserFileImporter userFileImporter;

  protected ExcelWorkbook excelWorkbook;

  public ExcelFileTemplate(File file) throws IOException {

    ExcelFileTransfer excelFileTransfer = new ExcelFileTransferImpl();

    this.excelWorkbook = excelFileTransfer.transfer(new FileInputStream(file));

    this.userFileGenerator = new ExcelFileGenerator(file);

    this.userFileImporter = new ExcelFileImporter(excelWorkbook);

    this.userFileValidator = new ExcelFileValidator(excelWorkbook, file);
  }

  @Override
  public Set<String> getCellValuesOfField(String field) {
    if (excelWorkbook == null) {
      return Collections.emptySet();
    }

    return excelWorkbook.getFirstSheet().getDistinctCellValuesOfField(field);
  }

  @Override
  public UserFileGenerator getUserFileGenerator() {
    return this.userFileGenerator;
  }

  @Override
  public UserFileValidator getUserFileValidator() {
    return this.userFileValidator;
  }

  @Override
  public UserFileImporter getUserFileImporter() {
    return this.userFileImporter;
  }

}