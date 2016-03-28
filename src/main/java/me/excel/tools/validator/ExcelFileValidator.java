package me.excel.tools.validator;

import me.excel.tools.exporter.ExcelCommentUtils;
import me.excel.tools.factory.FileTemplate;
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
public class ExcelFileValidator extends ExcelFileTransfer implements UserFileValidator {

  protected FileTemplate fileTemplate;

  protected List<ErrorMessage> errorMessages = new ArrayList<>();

  public ExcelFileValidator(FileTemplate fileTemplate) {
    this.fileTemplate = fileTemplate;
  }

  @Override
  public boolean validate(File excel) throws IOException {
    InputStream inputStream = new FileInputStream(excel);
    transfer(inputStream);

    validateWorkbook(excelWorkbook);

    if (!errorMessages.isEmpty()) {
      return false;
    }

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

  public void validateWorkbook(ExcelWorkbook workbook) {
    if (workbook == null) {
      throw new IllegalArgumentException("workbook is null");
    }
    if (workbook.getSheets().isEmpty()) {
      throw new IllegalArgumentException("sheet is null");
    }
    if (workbook.getSheet(0).getRows().isEmpty()) {
      throw new IllegalArgumentException("row is null");
    }
    ExcelCell firstCell = workbook.getSheet(0).getRow(0).getCell(0);
    if (workbook.getSheets().size() != 1) {
      this.errorMessages.add(new ErrorMessage(firstCell, "只支持单sheet导入"));
    }
    ExcelSheet sheet = workbook.getSheet(0);
    List<String> keyRowFields = sheet.getRow(1).getCells().stream()
        .map(excelCell -> excelCell.getValue()).collect(Collectors.toList());

    this.errorMessages.addAll(keyRowFields.stream()
        .filter(keyRowField -> !fileTemplate.getFieldScope().contains(keyRowField))
        .map(keyRowField -> new ErrorMessage(firstCell, keyRowField + ":字段不在处理范围内"))
        .collect(Collectors.toList()));

    this.errorMessages.addAll(fileTemplate.getRequiredFields().stream()
        .filter(requiredField -> !keyRowFields.contains(requiredField))
        .map(requiredField -> new ErrorMessage(firstCell, "不包含所要求的字段:"+requiredField))
        .collect(Collectors.toList()));
  }

  public void validateCell(ExcelCell cell) {
    if (cell == null) {
      return;
    }

    for (FieldValidator fieldValidator : fileTemplate.getValidators()) {

      if (!fieldValidator.matches(cell)) {
        continue;
      }

      try {
        if (!fieldValidator.validate(cell)) {
          this.errorMessages.add(new ErrorMessage(cell, fieldValidator.getErrorMessage()));
        }
      } catch (SkipValidateException e) {
        this.errorMessages.add(new ErrorMessage(e.getCell(), e.getPrompt()));
      }
    }
  }
}
