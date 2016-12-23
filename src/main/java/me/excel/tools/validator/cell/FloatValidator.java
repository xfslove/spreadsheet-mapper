package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCell;

/**
 * float validator
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class FloatValidator extends CellValidatorAdapter {

  public FloatValidator(String matchField) {
    super(matchField, "应该为小数");
  }

  public FloatValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {
    try {
      Float.parseFloat(excelCell.getValue());
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }

}
