package me.excel.tools.validator.data.workbook;

import me.excel.tools.model.excel.ExcelWorkbook;
import me.excel.tools.model.message.DataValidateMessage;
import me.excel.tools.validator.data.DataValidator;

/**
 * excel workbook values validator.
 * <p>
 * Created by hanwen on 2016/12/27.
 */
public interface WorkbookValidator extends DataValidator {

  /**
   * validate supplied excel workbook
   *
   * @param excelWorkbook workbook
   * @return result
   */
  DataValidateMessage validate(ExcelWorkbook excelWorkbook);
}
