package me.excel.tools.factory;

import me.excel.tools.importer.ExcelFileImporter;
import me.excel.tools.importer.UserFileImporter;
import me.excel.tools.model.excel.ExcelWorkbook;
import me.excel.tools.transfer.ExcelFileTransfer;
import me.excel.tools.transfer.ExcelFileTransferImpl;
import me.excel.tools.validator.ExcelFileValidator;
import me.excel.tools.validator.UserFileValidator;
import me.excel.tools.validator.cell.CellValidator;
import me.excel.tools.validator.row.RowValidator;
import me.excel.tools.validator.workbook.FieldScopeValidator;
import me.excel.tools.validator.workbook.RequireFieldValidator;
import me.excel.tools.validator.workbook.SheetSizeValidator;
import me.excel.tools.validator.workbook.WorkbookValidator;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 * excel 模板工厂, 只支持单sheet的导入，多sheet不支持
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class ExcelTemplate implements FileTemplate {

  protected List<String> fieldScope = new ArrayList<>();

  protected List<String> requiredFields = new ArrayList<>();

  protected List<CellValidator> cellValidators = new ArrayList<>();

  protected List<RowValidator> rowValidators = new ArrayList<>();

  protected List<WorkbookValidator> workbookValidators = new ArrayList<>();

  protected UserFileFactory userFileFactory;

  protected UserFileValidator userFileValidator;

  protected UserFileImporter userFileImporter;

  protected ExcelWorkbook excelWorkbook;

  public ExcelTemplate(File file) throws IOException {

    ExcelFileTransfer excelFileTransfer = new ExcelFileTransferImpl();

    this.excelWorkbook = excelFileTransfer.transfer(new FileInputStream(file));

    addWorkbookValidator(
        new SheetSizeValidator(1),
        new FieldScopeValidator(this.fieldScope),
        new RequireFieldValidator(this.requiredFields)
    );

    this.userFileFactory = new ExcelFileFactory(this, file);

    this.userFileImporter = new ExcelFileImporter(this, excelWorkbook);

    this.userFileValidator = new ExcelFileValidator(this, excelWorkbook, file);
  }

  @Override
  public void setFieldScope(String... fields) {
    if (fields == null) {
      throw new IllegalArgumentException("field scope is null");
    }

    for (String field : fields) {
      this.fieldScope.add(field);
    }
  }

  @Override
  public void setRequiredFields(String... fields) {
    if (fields == null) {
      throw new IllegalArgumentException("required field is null");
    }

    for (String field : fields) {
      this.requiredFields.add(field);
    }
  }

  @Override
  public void addCellValidator(CellValidator... validators) {
    if (validators == null) {
      return;
    }
    for (CellValidator validator : validators) {
      this.cellValidators.add(validator);
    }
  }

  @Override
  public void addRowValidator(RowValidator... validators) {
    if (validators == null) {
      return;
    }
    for (RowValidator validator : validators) {
      this.rowValidators.add(validator);
    }
  }

  @Override
  public void addWorkbookValidator(WorkbookValidator... validators) {
    if (validators == null) {
      return;
    }
    for (WorkbookValidator validator : validators) {
      this.workbookValidators.add(validator);
    }
  }

  @Override
  public Set<String> getCellValuesOfField(String field) {
    if (excelWorkbook == null) {
      return Collections.emptySet();
    }

    return excelWorkbook.getFirstSheet().getDistinctCellValuesOfField(field);
  }

  @Override
  public List<String> getFieldScope() {
    return this.fieldScope;
  }

  @Override
  public List<String> getRequiredFields() {
    return this.requiredFields;
  }

  @Override
  public List<CellValidator> getCellValidators() {
    return this.cellValidators;
  }

  @Override
  public List<WorkbookValidator> getWorkbookValidators() {
    return this.workbookValidators;
  }

  @Override
  public List<RowValidator> getRowValidators() {
    return this.rowValidators;
  }

  @Override
  public UserFileFactory getUserFileFactory() {
    return this.userFileFactory;
  }

  @Override
  public UserFileValidator getUserFileValidator() {
    return this.userFileValidator;
  }

  public UserFileImporter getUserFileImporter() {
    return this.userFileImporter;
  }

}