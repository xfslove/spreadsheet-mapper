package me.excel.tools.validator;

import me.excel.tools.exporter.ExcelCommentUtils;
import me.excel.tools.factory.FileTemplate;
import me.excel.tools.model.excel.*;
import me.excel.tools.model.message.ErrorMessage;
import me.excel.tools.transfer.ExcelFileTransfer;
import me.excel.tools.validator.cell.CellValidator;
import me.excel.tools.validator.row.RowValidator;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileValidator implements UserFileValidator {

  protected FileTemplate importTemplate;

  protected ExcelFileTransfer excelFileTransfer;

  protected List<ErrorMessage> errorMessages = new ArrayList<>();

  public ExcelFileValidator(FileTemplate fileTemplate, ExcelFileTransfer excelFileTransfer) {
    this.importTemplate = fileTemplate;
    this.excelFileTransfer = excelFileTransfer;
  }

  @Override
  public boolean validate(File excel) throws IOException {

    if (excel == null) {
      throw new IllegalArgumentException("excel is null");
    }

    InputStream inputStream = new FileInputStream(excel);
    ExcelWorkbook excelWorkbook = excelFileTransfer.transfer(inputStream);

    validateWorkbook(excelWorkbook);
    if (!errorMessages.isEmpty()) {
      return false;
    }

    excelWorkbook.getSheet(0).getDataRows()
        .forEach(row -> row.getCells()
            .forEach(cell -> validateCell(cell)));

    if (!errorMessages.isEmpty()) {
      return false;
    }

    excelWorkbook.getSheet(0).getDataRows().forEach(row -> validateRow(row));

    if (!errorMessages.isEmpty()) {
      return false;
    }

    return true;
  }

  @Override
  public void writeFailureMessageComments(File excel) throws IOException {

    if (errorMessages.isEmpty()) {
      return;
    }

    List<ExcelCellComment> commentList = new ArrayList<>();
    errorMessages.forEach(errorMessage -> {
      ExcelCell excelCell = errorMessage.getCell();
      ExcelCellComment excelCellComment;
      if (excelCell.getComment() == null) {
        excelCellComment = new ExcelCellCommentBean();
        excelCell.setComment(excelCellComment);
      }
      excelCellComment = excelCell.getComment();
      excelCellComment.addComment(errorMessage.getContent());
      commentList.add(excelCellComment);
    });
    ExcelCommentUtils.writeToFile(excel, commentList);
  }

  private void validateCell(ExcelCell cell) {
    if (cell == null) {
      return;
    }

    for (CellValidator cellValidator : importTemplate.getCellValidators()) {

      if (!cellValidator.matches(cell)) {
        continue;
      }

      try {
        if (!cellValidator.validate(cell)) {
          errorMessages.add(new ErrorMessage(cell, cellValidator.getErrorMessage()));
        }
      } catch (SkipValidateException e) {
        errorMessages.addAll(e.getCells().stream().map(excelCell -> new ErrorMessage(excelCell, e.getPrompt())).collect(Collectors.toList()));
      }
    }
  }

  private void validateRow(ExcelRow row) {
    if (row == null) {
      return;
    }

    for (RowValidator rowValidator : importTemplate.getRowValidators()) {

      try {
        if (!rowValidator.validate(row)) {

          errorMessages.addAll(rowValidator.getMessageOnCells(row).stream()
              .map(excelCell -> new ErrorMessage(excelCell, rowValidator.getErrorMessage())).collect(Collectors.toList()));
        }
      } catch (SkipValidateException e) {
        errorMessages.addAll(e.getCells().stream().map(excelCell -> new ErrorMessage(excelCell, e.getPrompt())).collect(Collectors.toList()));
      }
    }
  }

  private void validateWorkbook(ExcelWorkbook workbook) {
    errorMessages.addAll(
        importTemplate.getWorkbookValidators().stream()
            .filter(workbookValidator -> !workbookValidator.validate(workbook))
            .map(workbookValidator -> new ErrorMessage(workbookValidator.getMessageOnCell(workbook), workbookValidator.getErrorMessage()))
            .collect(Collectors.toList()));
  }
}
