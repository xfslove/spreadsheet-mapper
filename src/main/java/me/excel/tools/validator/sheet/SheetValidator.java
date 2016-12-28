package me.excel.tools.validator.sheet;

import me.excel.tools.model.excel.Sheet;
import me.excel.tools.validator.Validator;

/**
 * excel sheet validator, after workbook validators, if workbook validators failure, sheet validators will skip.
 * <p>
 * Created by hanwen on 2016/12/23.
 */
public interface SheetValidator extends Validator {

  /**
   * valid supplied excel sheet
   *
   * @param sheet sheet
   * @return result
   */
  boolean validate(Sheet sheet);
}
