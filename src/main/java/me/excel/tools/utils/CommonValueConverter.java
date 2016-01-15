package me.excel.tools.utils;


import me.excel.tools.model.excel.ExcelCell;

import java.util.function.Function;

/**
 * Created by hanwen on 16-1-15.
 */
public class CommonValueConverter extends AbstractCellValueConverter {

  protected Function<ExcelCell, String> readableValueGetter;

  public CommonValueConverter(String matchedField, Function<ExcelCell, String> readableValueGetter) {
    super(matchedField);
    this.readableValueGetter = readableValueGetter;
  }

  @Override
  public String getReadableValue(ExcelCell excelCell) {
    return readableValueGetter.apply(excelCell);
  }

}
