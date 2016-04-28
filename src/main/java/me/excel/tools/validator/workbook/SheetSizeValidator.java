package me.excel.tools.validator.workbook;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelWorkbook;

/**
 * Created by hanwen on 4/26/16.
 */
public class SheetSizeValidator implements WorkbookValidator {

  protected int size;

  public SheetSizeValidator(int size) {
    this.size = size;
  }

  @Override
  public String getErrorMessage() {
    return "只支持单sheet导入";
  }

  @Override
  public ExcelCell getMessageOnCell(ExcelWorkbook excelWorkbook) {
    return excelWorkbook.getFirstSheet().getRow(0).getCell(0);
  }

  @Override
  public boolean validate(ExcelWorkbook excelWorkbook) {
    if (excelWorkbook == null) {
      throw new IllegalArgumentException("workbook is null");
    }
    if (excelWorkbook.sizeOfSheets() == 0) {
      throw new IllegalArgumentException("sheet is null");
    }
    if (excelWorkbook.getFirstSheet().sizeOfRows() == 0) {
      throw new IllegalArgumentException("row is null");
    }
    return excelWorkbook.sizeOfSheets() == size;
  }
}
