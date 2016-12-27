package me.excel.tools.validator.data.row;


import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.model.message.DataValidateMessage;
import me.excel.tools.validator.data.DataValidator;

/**
 * excel row values validator, after sheet validators.
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface RowValidator extends DataValidator {

  /**
   * validate supplied excel row
   *
   * @param excelRow row
   * @return result
   */
  DataValidateMessage validate(ExcelRow excelRow);
}
