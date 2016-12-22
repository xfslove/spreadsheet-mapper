package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCell;

/**
 * long validator
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LongValidator extends CellValidatorAdapter {

  public LongValidator(String field) {
    super(field, "应该为整数");
  }

  public LongValidator(String field, String message) {
    super(field, message);
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
