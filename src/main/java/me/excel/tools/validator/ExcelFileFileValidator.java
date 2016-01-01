package me.excel.tools.validator;

import me.excel.tools.exporter.ExcelCommentUtils;
import me.excel.tools.factory.ImportTemplate;
import me.excel.tools.model.excel.*;
import me.excel.tools.model.message.ErrorMessage;
import me.excel.tools.transfer.ExcelFileTransfer;

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
public class ExcelFileFileValidator extends ExcelFileTransfer implements UserFileValidator {

  protected ImportTemplate importTemplate;

  protected List<ErrorMessage> errorMessages = new ArrayList<>();

  public ExcelFileFileValidator(ImportTemplate importTemplate) {
    this.importTemplate = importTemplate;
  }

  @Override
  public boolean validate(File excel) throws IOException {
    InputStream inputStream = new FileInputStream(excel);
    transfer(inputStream);

    validateWorkbook(excelWorkbook);

    excelWorkbook.getSheet(0).getDataRows()
            .forEach(row -> row.getCells()
            .forEach(cell -> validateCell(cell)));

    if (!this.errorMessages.isEmpty()) {
      return false;
    }
    return true;
  }

  @Override
  public void writeFailureMessageComments(File excel) throws IOException {

    if (excelWorkbook == null) {
      throw new IllegalArgumentException("workbook is null");
    }
    if (errorMessages.isEmpty()) {
      return;
    }

    List<ExcelCellComment> excelCellComments = new ArrayList<>();
    ExcelSheet sheet = excelWorkbook.getSheet(0);
    sheet.getRows()
        .forEach(row -> row.getCells()
        .forEach(cell -> errorMessages.stream()
            .filter(errorMessage -> errorMessage.matches(cell))
            .forEach(errorMessage -> {
              cell.setComment(new ExcelCellCommentBean(errorMessage.getErrorMessage()));
              excelCellComments.add(cell.getComment());
            })
        ));
    ExcelCommentUtils.writeToFile(excel, excelCellComments);
  }

  public void validateWorkbook(ExcelWorkbook workbook) {
    if (workbook == null) {
      throw new IllegalArgumentException("workbook is null");
    }
    if (workbook.getSheets().size() == 0) {
      throw new IllegalArgumentException("sheet is null");
    }
    if (workbook.getSheet(0).getRows().size() == 0) {
      throw new IllegalArgumentException("row is null");
    }
    ExcelCell firstCell = workbook.getSheet(0).getRow(0).getCell(0);
    if (workbook.getSheets().size() != 1) {
      this.errorMessages.add(new ErrorMessage(firstCell, "只支持单sheet导入"));
    }
    ExcelSheet sheet = workbook.getSheet(0);
    List<ExcelCell> keyRowCells = sheet.getRow(1).getCells();

    this.errorMessages.addAll(keyRowCells.stream()
        .filter(keyRowCell -> !importTemplate.getFieldScope().contains(keyRowCell))
        .map(keyRowCell -> new ErrorMessage(firstCell, keyRowCell.getField()+":字段不在处理范围内"))
        .collect(Collectors.toList()));

    this.errorMessages.addAll(importTemplate.getRequiredFields().stream()
        .filter(requiredField -> !keyRowCells.contains(requiredField))
        .map(requiredField -> new ErrorMessage(firstCell, "不包含所要求的字段:"+requiredField))
        .collect(Collectors.toList()));
  }

  public void validateCell(ExcelCell cell) {
    if (cell == null) {
      return;
    }

    this.errorMessages.addAll(importTemplate.getValidators().stream()
        .filter(fieldValidator -> fieldValidator.matches(cell))
        .filter(fieldValidator -> !fieldValidator.validate(cell))
        .map(fieldValidator -> new ErrorMessage(cell, fieldValidator.getErrorMessage()))
        .collect(Collectors.toList()));
  }
}
