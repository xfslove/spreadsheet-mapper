package me.excel.tools.validator;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.model.excel.ExcelSheet;
import me.excel.tools.model.excel.ExcelWorkbook;
import me.excel.tools.model.message.ErrorMessage;
import me.excel.tools.validator.cell.CellValidator;
import me.excel.tools.validator.row.RowValidator;
import me.excel.tools.validator.sheet.SheetValidator;
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

  private List<CellValidator> cellValidators = new ArrayList<>();

  private List<RowValidator> rowValidators = new ArrayList<>();

  private List<SheetValidator> sheetValidators = new ArrayList<>();

  private List<WorkbookValidator> workbookValidators = new ArrayList<>();

  private List<ErrorMessage> errorMessages = new ArrayList<>();

  private ExcelWorkbook excelWorkbook;

  public ExcelFileValidator(ExcelWorkbook excelWorkbook) {
    this.excelWorkbook = excelWorkbook;
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
  public void addSheetValidator(SheetValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.sheetValidators, validators);
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
      throw new IllegalArgumentException("excel workbook is null");
    }

    validateWorkbook(excelWorkbook);
    if (!errorMessages.isEmpty()) {
      return false;
    }

    for (ExcelSheet excelSheet : excelWorkbook.getSheets()) {
      validateSheet(excelSheet);
      if (!errorMessages.isEmpty()) {
        return false;
      }
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

  private void validateWorkbook(ExcelWorkbook workbook) {

    for (WorkbookValidator workbookValidator : workbookValidators) {

      if (!workbookValidator.validate(workbook)) {

        for (ExcelCell excelCell : workbookValidator.getMessageOnCells(workbook)) {
          errorMessages.add(new ErrorMessage(excelCell, workbookValidator.getErrorMessage()));
        }
      }
    }
  }

  private void validateSheet(ExcelSheet sheet) {

    for (SheetValidator sheetValidator : sheetValidators) {

      if (!sheetValidator.validate(sheet)) {

        for (ExcelCell excelCell : sheetValidator.getMessageOnCells(sheet)) {
          errorMessages.add(new ErrorMessage(excelCell, sheetValidator.getErrorMessage()));
        }
      }
    }
  }

  private void validateCell(ExcelCell cell) {
    if (cell == null) {
      return;
    }

    for (CellValidator cellValidator : cellValidators) {

      if (!cellValidator.matches(cell.getField())) {
        continue;
      }

      if (!cellValidator.validate(cell)) {
        errorMessages.add(new ErrorMessage(cell, cellValidator.getErrorMessage()));
      }
    }
  }

  private void validateRow(ExcelRow row) {
    if (row == null) {
      return;
    }

    for (RowValidator rowValidator : rowValidators) {

      if (!rowValidator.validate(row)) {

        errorMessages.addAll(rowValidator.getMessageOnCells(row).stream()
            .filter(Objects::nonNull)
            .map(excelCell -> new ErrorMessage(excelCell, rowValidator.getErrorMessage())).collect(Collectors.toList()));
      }
    }
  }
}
