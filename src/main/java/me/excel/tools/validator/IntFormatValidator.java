package me.excel.tools.validator;

import me.excel.tools.model.excel.ExcelCell;

/**
 * Created by hanwen on 15-12-16.
 */
public class IntFormatValidator extends AbstractFieldValidator {

  public IntFormatValidator(String field) {
    super(field, "应该为整数", "整数");
  }

  public IntFormatValidator(String field, String message, String prompt) {
    super(field, message, prompt);
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {
    return isValidInt(excelCell.getValue());
  }

  private boolean isValidInt(String value) {
    try {
      Integer.parseInt(value);
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }
}
