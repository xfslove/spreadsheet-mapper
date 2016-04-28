package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.ExcelCell;

/**
 * Created by hanwen on 15-12-16.
 */
public class DoubleFormatValidator extends AbstractCellValidator {

  public DoubleFormatValidator(String field) {
    super(field, "应该为小数", "小数");
  }

  public DoubleFormatValidator(String field, String message, String prompt) {
    super(field, message, prompt);
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {
    return isValidDouble(excelCell.getValue());
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
