package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.ExcelCell;

/**
 * excel cell value validator
 * <p>
 * Created by hanwen on 15-12-15.
 */
public interface CellValidator {

  /**
   * get validate error message
   *
   * @return
   */
  String getErrorMessage();

  /**
   * validate supplied excel cell
   *
   * @param excelCell
   * @return
   */
  boolean validate(ExcelCell excelCell);

  /**
   * matches which field
   *
   * @param field
   * @return
   */
  boolean matches(String field);
}
