package me.excel.tools.utils;


import me.excel.tools.model.excel.ExcelCell;

/**
 * Created by hanwen on 16-1-15.
 */
public abstract class AbstractCellValueConverter implements CellValueConverter {

  protected String matchedField;

  public AbstractCellValueConverter(String matchedField) {
    this.matchedField = matchedField;
  }

  @Override
  public abstract String getReadableValue(ExcelCell excelCell);

  @Override
  public boolean matches(ExcelCell excelCell) {
    return matchedField.equals(excelCell.getField());
  }

  @Override
  public boolean matches(String field) {
    return matchedField.equals(field);
  }
}
