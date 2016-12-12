package me.excel.tools.validator.row;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.validator.SkipValidateException;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by hanwen on 4/26/16.
 */
public abstract class AbstractRowValidator implements RowValidator {

  protected String prompt;

  protected String errorMessage;

  protected List<String> messageOnFields = new ArrayList<>();

  public AbstractRowValidator(String prompt, String errorMessage, String[] messageOnFields) {
    this.prompt = prompt;
    this.errorMessage = errorMessage;
    for (String messageOnField : messageOnFields) {
      this.messageOnFields.add(messageOnField);
    }
  }

  @Override
  public String getPrompt() {
    return prompt;
  }

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }

  @Override
  public List<String> getMessageOnFields() {
    return messageOnFields;
  }

  @Override
  public List<ExcelCell> getMessageOnCells(ExcelRow excelRow) {

    return messageOnFields.stream().map(excelRow::getCell).collect(Collectors.toList());
  }

  @Override
  public boolean validate(ExcelRow excelRow) throws SkipValidateException {
    return customValidate(excelRow);
  }

  protected abstract boolean customValidate(ExcelRow excelRow) throws SkipValidateException;
}
