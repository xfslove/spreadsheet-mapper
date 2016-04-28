package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.validator.SkipValidateException;

/**
 * Created by hanwen on 15-12-15.
 */
public interface CellValidator {

  String getErrorMessage();

  String getPrompt();

  boolean validate(ExcelCell excelCell) throws SkipValidateException;

  boolean matches(ExcelCell excelCell);

  boolean matches(String field);
}
