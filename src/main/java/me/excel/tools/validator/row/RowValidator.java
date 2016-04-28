package me.excel.tools.validator.row;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.validator.SkipValidateException;

import java.util.List;

/**
 * Created by hanwen on 4/26/16.
 */
public interface RowValidator {

  String getPrompt();

  String getErrorMessage();

  List<String> getMessageOnFields();

  List<ExcelCell> getMessageOnCells(ExcelRow excelRow);

  boolean validate(ExcelRow excelRow) throws SkipValidateException;
}
