package me.excel.tools.validator.data.sheet;

import me.excel.tools.model.excel.ExcelSheet;
import me.excel.tools.model.message.DataValidateMessage;
import me.excel.tools.validator.data.DataValidator;

/**
 * excel sheet values validator, after workbook validators.
 * <p>
 * Created by hanwen on 2016/12/27.
 */
public interface SheetValidator extends DataValidator {

  /**
   * validate supplied excel sheet
   *
   * @param excelSheet sheet
   * @return result
   */
  DataValidateMessage validate(ExcelSheet excelSheet);
}
