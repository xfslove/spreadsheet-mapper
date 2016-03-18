package me.excel.tools.utils;

import me.excel.tools.model.excel.ExcelCell;

/**
 * Created by hanwen on 16/3/18.
 */
public class BooleanConverter implements CellValueConverter {

  @Override
  public String getReadableValue(ExcelCell excelCell) {
    if ("true".equals(excelCell.getValue())) {
      return "是";
    } else if ("false".equals(excelCell.getValue())) {
      return "否";
    }
    return "";
  }

  @Override
  public boolean matches(ExcelCell excelCell) {
    return "true".equals(excelCell.getValue()) || "false".equals(excelCell.getValue());
  }

  @Override
  public boolean matches(String field) {
    return false;
  }
}
