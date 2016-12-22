package me.excel.tools.setter;


import me.excel.tools.model.excel.ExcelCell;

/**
 * model field value setter
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface FieldValueSetter {

  /**
   * set object field from cell value
   *
   * @param data
   * @param excelCell
   */
  void set(Object data, ExcelCell excelCell);

  /**
   * matches which field
   *
   * @param field
   * @return
   */
  boolean matches(String field);
}
