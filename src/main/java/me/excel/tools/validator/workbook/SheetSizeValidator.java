package me.excel.tools.validator.workbook;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelWorkbook;

import java.util.Collections;
import java.util.List;

/**
 * sheet size validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public class SheetSizeValidator implements WorkbookValidator {

  private int size;

  public SheetSizeValidator(int size) {
    this.size = size;
  }

  @Override
  public String getErrorMessage() {
    return "工作表数量不是" + size + "个";
  }

  @Override
  public List<ExcelCell> getMessageOnCells(ExcelWorkbook excelWorkbook) {
    return Collections.singletonList(excelWorkbook.getFirstSheet().getFirstRow().getFirstCell());
  }

  @Override
  public boolean validate(ExcelWorkbook excelWorkbook) {
    return excelWorkbook.sizeOfSheets() == size;
  }
}
