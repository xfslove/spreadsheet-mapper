package me.excel.tools.validator.template.workbook;


import me.excel.tools.model.excel.ExcelWorkbook;
import me.excel.tools.validator.template.TemplateValidator;

/**
 * excel workbook template validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface WorkbookTemplateValidator extends TemplateValidator {

  /**
   * validate supplied excel workbook
   *
   * @param excelWorkbook workbook
   * @return success
   */
  boolean validate(ExcelWorkbook excelWorkbook);
}
