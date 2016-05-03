package me.excel.tools.factory;

import me.excel.tools.ExcelSupportedDateFormat;
import me.excel.tools.extractor.BooleanExtractor;
import me.excel.tools.extractor.LocalDateExtractor;
import me.excel.tools.extractor.LocalDateTimeExtractor;
import me.excel.tools.importer.ExcelFileImporter;
import me.excel.tools.importer.UserFileImporter;
import me.excel.tools.model.excel.ExcelWorkbook;
import me.excel.tools.setter.BooleanValueSetter;
import me.excel.tools.setter.LocalDateTimeValueSetter;
import me.excel.tools.setter.LocalDateValueSetter;
import me.excel.tools.transfer.ExcelFileTransfer;
import me.excel.tools.transfer.ExcelFileTransferImpl;
import me.excel.tools.validator.ExcelFileValidator;
import me.excel.tools.validator.UserFileValidator;
import me.excel.tools.validator.cell.BooleanValidator;
import me.excel.tools.validator.cell.CellValidator;
import me.excel.tools.validator.cell.LocalDateTimeValidator;
import me.excel.tools.validator.cell.LocalDateValidator;
import me.excel.tools.validator.row.RowValidator;
import me.excel.tools.validator.workbook.*;
import org.apache.commons.lang3.StringUtils;

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

  protected int minFieldCount;

  protected List<String> fieldScope = new ArrayList<>();

  protected List<String> requiredFields = new ArrayList<>();

  protected List<CellValidator> cellValidators = new ArrayList<>();

  protected List<RowValidator> rowValidators = new ArrayList<>();

  protected List<WorkbookValidator> workbookValidators = new ArrayList<>();

  protected UserFileFactory userFileFactory;

  protected UserFileValidator userFileValidator;

  protected UserFileImporter userFileImporter;

  protected ExcelFileTransfer excelFileTransfer;

  public ExcelTemplate() {

    this.excelFileTransfer = new ExcelFileTransferImpl();

    addWorkbookValidator(
        new SheetSizeValidator(1),
        new FieldScopeValidator(this.fieldScope),
        new RequireFieldValidator(this.requiredFields),
        new FieldCountValidator(this.minFieldCount)
    );

    this.userFileFactory = new ExcelFileFactory(this);

    this.userFileImporter = new ExcelFileImporter(excelFileTransfer);

    this.userFileValidator = new ExcelFileValidator(this, excelFileTransfer);
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
  public void setMinFieldCount(int count) {
    this.minFieldCount = count;
  }

  @Override
  public void addCellValidator(CellValidator... validators) {
    if (validators == null) {
      return;
    }
    for (CellValidator validator : validators) {
      this.cellValidators.add(validator);
      addDefaultValueExtractor(validator);
      addDefaultValueSetter(validator);
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
  public Set<String> getCellValuesOfField(File excel, String field) throws IOException {
    if (excel == null) {
      return Collections.emptySet();
    }

    FileInputStream inputStream = new FileInputStream(excel);
    ExcelWorkbook excelWorkbook = excelFileTransfer.transfer(false, inputStream);

    return excelWorkbook.getFirstSheet().getDistinctCellValuesOfField(field);
  }

  @Override
  public int getMinFieldCount() {
    return this.minFieldCount;
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

  private void addDefaultValueSetter(CellValidator validator) {
    String matchField = validator.getMatchField();
    String dateTimePattern = extraPatternFromPrompt(validator.getPrompt());
    if (validator instanceof BooleanValidator) {
      userFileImporter.addCellValueSetter(new BooleanValueSetter(matchField));
    } else if (validator instanceof LocalDateValidator) {
      userFileImporter.addCellValueSetter(new LocalDateValueSetter(matchField, dateTimePattern));
    } else if (validator instanceof LocalDateTimeValidator) {
      userFileImporter.addCellValueSetter(new LocalDateTimeValueSetter(matchField, dateTimePattern));
    }
  }

  private void addDefaultValueExtractor(CellValidator validator) {
    String matchField = validator.getMatchField();
    String dateTimePattern = extraPatternFromPrompt(validator.getPrompt());
    if (validator instanceof BooleanValidator) {
      userFileFactory.addValueExtractors(new BooleanExtractor(matchField));
    } else if (validator instanceof LocalDateValidator) {
      userFileFactory.addValueExtractors(new LocalDateExtractor(matchField, dateTimePattern));
    } else if (validator instanceof LocalDateTimeValidator) {
      userFileFactory.addValueExtractors(new LocalDateTimeExtractor(matchField, dateTimePattern));
    }
  }

  private String extraPatternFromPrompt(String prompt) {

    for (String supportedFormat : ExcelSupportedDateFormat.getSupportedFormats()) {
      if (StringUtils.indexOfIgnoreCase(prompt, supportedFormat) != -1) {
        return supportedFormat;
      }
    }

    return null;
  }
}