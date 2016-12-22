package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.ExcelCell;

/**
 * int validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class IntValidator extends CellValidatorAdapter {

  public IntValidator(String field) {
    super(field, "应该为整数");
  }

  public IntValidator(String field, String message) {
    super(field, message);
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {
    try {
      Integer.parseInt(excelCell.getValue());
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }
}
