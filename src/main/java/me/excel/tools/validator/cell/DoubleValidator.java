package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.ExcelCell;

/**
 * double validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class DoubleValidator extends CellValidatorAdapter {

  public DoubleValidator(String field) {
    super(field, "应该为小数");
  }

  public DoubleValidator(String field, String message) {
    super(field, message);
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {
    try {
      Double.parseDouble(excelCell.getValue());
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }
}
