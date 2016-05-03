package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.validator.SkipValidateException;

/**
 * Created by hanwen on 5/3/16.
 */
public class LongValidator extends AbstractCellValidator {

  public LongValidator(String field) {
    super(field, "应该为整数", "整数");
  }

  public LongValidator(String field, String message, String prompt) {
    super(field, message, prompt);
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) throws SkipValidateException {
    try {
      Long.parseLong(excelCell.getValue());
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }
}
