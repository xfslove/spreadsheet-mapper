package me.excel.tools.validator.workbook;


import me.excel.tools.model.excel.Workbook;
import me.excel.tools.validator.Validator;

/**
 * excel workbook template validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface WorkbookValidator extends Validator {

  /**
   * valid supplied excel workbook
   *
   * @param workbook workbook
   * @return success
   */
  boolean validate(Workbook workbook);
}
