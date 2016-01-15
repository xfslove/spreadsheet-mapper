package me.excel.tools.utils;


import me.excel.tools.model.excel.ExcelCell;

/**
 * cell value converter
 *
 * Created by hanwen on 15-12-16.
 */
public interface CellValueConverter {

  String getReadableValue(ExcelCell excelCell);

  boolean matches(ExcelCell excelCell);

  boolean matches(String field);
}
