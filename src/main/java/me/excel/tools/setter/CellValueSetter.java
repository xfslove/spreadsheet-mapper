package me.excel.tools.setter;


import me.excel.tools.model.excel.ExcelCell;

/**
 * model field value setter
 *
 * Created by hanwen on 15-12-16.
 */
public interface CellValueSetter {

  String getMatchField();

  void set(Object data, ExcelCell excelCell);

  boolean matches(ExcelCell excelCell);
}
