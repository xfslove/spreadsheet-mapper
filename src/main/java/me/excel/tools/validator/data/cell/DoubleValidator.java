package me.excel.tools.validator.data.cell;


import me.excel.tools.model.excel.ExcelCell;

/**
 * double validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class DoubleValidator extends CellValidatorAdapter {

  public DoubleValidator(String matchField) {
    super(matchField, "应该为小数");
  }

  public DoubleValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
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
