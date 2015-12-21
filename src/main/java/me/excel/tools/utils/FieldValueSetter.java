package me.excel.tools.utils;

import me.excel.tools.model.excel.ExcelCell;

/**
 * model field value setter
 *
 * Created by hanwen on 15-12-16.
 */
public interface FieldValueSetter {

  void set(Object data, ExcelCell excelCell);

  boolean matches(ExcelCell excelCell);

  boolean matches(String field);
}
