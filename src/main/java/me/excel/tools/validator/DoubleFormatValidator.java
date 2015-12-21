package me.excel.tools.validator;

import me.excel.tools.model.excel.ExcelCell;

/**
 * Created by hanwen on 15-12-16.
 */
public class DoubleFormatValidator extends AbstractFieldValidator {

  public DoubleFormatValidator(String field) {
    super(field, "数据不正确, 应该为小数", "小数");
  }

  public DoubleFormatValidator(String field, String message, String prompt) {
    super(field, message, prompt);
  }

  @Override
  public boolean validate(ExcelCell excelCell) {
    String value = excelCell.getValue();
    if (value == null) {
      return false;
    }
    return isValidDouble(value);
  }

  private boolean isValidDouble(String value) {
    try {
      Double.parseDouble(value);
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }
}
