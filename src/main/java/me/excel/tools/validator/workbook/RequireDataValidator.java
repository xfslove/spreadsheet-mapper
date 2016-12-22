package me.excel.tools.validator.workbook;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelWorkbook;

import java.util.Collections;
import java.util.List;

/**
 * required import data validator
 * <p>
 * Created by hanwen on 2016/12/22.
 */
public class RequireDataValidator implements WorkbookValidator {

  @Override
  public String getErrorMessage() {
    return "导入模板中没有数据";
  }

  @Override
  public List<ExcelCell> getCausedByCells(ExcelWorkbook excelWorkbook) {
    return Collections.singletonList(excelWorkbook.getFirstSheet().getRow(1).getCell(1));
  }

  @Override
  public boolean validate(ExcelWorkbook excelWorkbook) {
    return excelWorkbook.getFirstSheet().getDataRows().size() > 0;
  }
}
