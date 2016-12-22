package me.excel.tools.validator;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.model.excel.ExcelWorkbook;
import me.excel.tools.model.message.ErrorMessage;
import me.excel.tools.validator.cell.CellValidator;
import me.excel.tools.validator.row.RowValidator;
import me.excel.tools.validator.workbook.FieldScopeValidator;
import me.excel.tools.validator.workbook.RequireFieldValidator;
import me.excel.tools.validator.workbook.SheetSizeValidator;
import me.excel.tools.validator.workbook.WorkbookValidator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileValidator implements UserFileValidator {

  private List<String> fieldScope = new ArrayList<>();

  private List<String> requiredFields = new ArrayList<>();

  private List<CellValidator> cellValidators = new ArrayList<>();

  private List<RowValidator> rowValidators = new ArrayList<>();

  private List<WorkbookValidator> workbookValidators = new ArrayList<>();

  private ExcelWorkbook excelWorkbook;

  private List<ErrorMessage> errorMessages = new ArrayList<>();

  public ExcelFileValidator(ExcelWorkbook excelWorkbook) {

    addWorkbookValidator(
        new SheetSizeValidator(1),
        new FieldScopeValidator(this.fieldScope),
        new RequireFieldValidator(this.requiredFields)
    );

    this.excelWorkbook = excelWorkbook;
  }

  @Override
  public void setFieldScope(String... fields) {
    if (fields == null) {
      throw new IllegalArgumentException("field scope is null");
    }

    Collections.addAll(this.fieldScope, fields);
  }

  @Override
  public void setRequiredFields(String... fields) {
    if (fields == null) {
      throw new IllegalArgumentException("required field is null");
    }

    Collections.addAll(this.requiredFields, fields);
  }

  @Override
  public void addCellValidator(CellValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.cellValidators, validators);
  }

  @Override
  public void addRowValidator(RowValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.rowValidators, validators);
  }

  @Override
  public void addWorkbookValidator(WorkbookValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.workbookValidators, validators);
  }

  @Override
  public boolean validate() {

    if (excelWorkbook == null) {
      throw new IllegalArgumentException("excel is null");
    }

    validateWorkbook(excelWorkbook);
    if (!errorMessages.isEmpty()) {
      return false;
    }

    excelWorkbook.getSheet(1).getDataRows()
        .forEach(row -> row.getCells()
            .forEach(this::validateCell));

    if (!errorMessages.isEmpty()) {
      return false;
    }

    excelWorkbook.getSheet(1).getDataRows().forEach(this::validateRow);

    return errorMessages.isEmpty();
  }

  @Override
  public List<ErrorMessage> getErrorMessages() {
    return errorMessages;
  }

  private void validateCell(ExcelCell cell) {
    if (cell == null) {
      return;
    }

    for (CellValidator cellValidator : cellValidators) {

      if (!cellValidator.matches(cell)) {
        continue;
      }

      try {
        if (!cellValidator.validate(cell)) {
          errorMessages.add(new ErrorMessage(cell, cellValidator.getErrorMessage()));
        }
      } catch (SkipValidateException e) {
        errorMessages.addAll(e.getCells().stream().map(excelCell -> new ErrorMessage(excelCell, e.getErrorMessage())).collect(Collectors.toList()));
      }
    }
  }

  private void validateRow(ExcelRow row) {
    if (row == null) {
      return;
    }

    for (RowValidator rowValidator : rowValidators) {

      try {
        if (!rowValidator.validate(row)) {

          errorMessages.addAll(rowValidator.getMessageOnCells(row).stream()
              .filter(Objects::nonNull)
              .map(excelCell -> new ErrorMessage(excelCell, rowValidator.getErrorMessage())).collect(Collectors.toList()));
        }
      } catch (SkipValidateException e) {
        errorMessages.addAll(e.getCells().stream().map(excelCell -> new ErrorMessage(excelCell, e.getErrorMessage())).collect(Collectors.toList()));
      }
    }
  }

  private void validateWorkbook(ExcelWorkbook workbook) {
    errorMessages.addAll(
        workbookValidators.stream()
            .filter(workbookValidator -> !workbookValidator.validate(workbook))
            .map(workbookValidator -> new ErrorMessage(workbookValidator.getMessageOnCell(workbook), workbookValidator.getErrorMessage()))
            .collect(Collectors.toList()));
  }
}
