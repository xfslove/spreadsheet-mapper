package me.excel.tools.setter;


import me.excel.tools.model.excel.ExcelCell;

/**
 * object field value setter
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface FieldValueSetter {

  /**
   * set object field from cell value
   *
   * @param data      supplied object
   * @param excelCell cell
   */
  void set(Object data, ExcelCell excelCell);

  /**
   * matches which field
   *
   * @param field field
   * @return success
   */
  boolean matches(String field);

  String getMatchField();
}
