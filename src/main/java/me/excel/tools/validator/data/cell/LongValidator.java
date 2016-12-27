package me.excel.tools.validator.data.cell;

import me.excel.tools.model.excel.ExcelCell;

/**
 * long validator
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LongValidator extends CellValidatorAdapter {

  public LongValidator(String matchField) {
    super(matchField, "应该为整数");
  }

  public LongValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {
    try {
      Long.parseLong(excelCell.getValue());
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }
}
