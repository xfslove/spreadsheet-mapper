package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCell;

/**
 * float validator
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class FloatValidator extends CellValidatorAdapter {

  public FloatValidator(String field) {
    super(field, "应该为小数");
  }

  public FloatValidator(String field, String message) {
    super(field, message);
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
