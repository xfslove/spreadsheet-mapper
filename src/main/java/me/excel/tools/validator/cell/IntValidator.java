package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.ExcelCell;

/**
 * Created by hanwen on 15-12-16.
 */
public class IntValidator extends AbstractCellValidator {

  public IntValidator(String field) {
    super(field, "应该为整数", "整数");
  }

  public IntValidator(String field, String message, String prompt) {
    super(field, message, prompt);
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