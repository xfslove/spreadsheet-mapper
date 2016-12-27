package me.excel.tools.validator.data.cell;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.message.DataValidateMessage;
import me.excel.tools.validator.data.DataValidator;

/**
 * excel cell value validator, after row validators.
 * <p>
 * Created by hanwen on 15-12-15.
 */
public interface CellValidator extends DataValidator {

  /**
   * validate supplied excel cell value
   *
   * @param excelCell cell
   * @return result
   */
  DataValidateMessage validate(ExcelCell excelCell);
}
