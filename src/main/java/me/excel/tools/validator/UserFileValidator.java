package me.excel.tools.validator;

import me.excel.tools.model.message.DataValidateMessage;
import me.excel.tools.model.message.TemplateValidateMessage;
import me.excel.tools.validator.data.cell.CellValidator;
import me.excel.tools.validator.data.row.RowValidator;
import me.excel.tools.validator.data.sheet.SheetValidator;
import me.excel.tools.validator.data.workbook.WorkbookValidator;
import me.excel.tools.validator.template.sheet.SheetTemplateValidator;
import me.excel.tools.validator.template.workbook.WorkbookTemplateValidator;

import java.util.List;

/**
 * excel file validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface UserFileValidator {

  /**
   * @param validators workbook template validator
   * @see WorkbookTemplateValidator
   */
  void addWorkbookTemplateValidator(WorkbookTemplateValidator... validators);

  /**
   * @param validators sheet template validator
   * @see SheetTemplateValidator
   */
  void addSheetTemplateValidator(SheetTemplateValidator... validators);

  /**
   * @param validators workbook validator
   * @see WorkbookTemplateValidator
   */
  void addWorkbookValidator(WorkbookValidator... validators);

  /**
   * @param validators sheet validator
   * @see SheetTemplateValidator
   */
  void addSheetValidator(SheetValidator... validators);

  /**
   * @param validators row validator
   * @see RowValidator
   */
  void addRowValidator(RowValidator... validators);

  /**
   * @param validators cell validator
   * @see CellValidator
   */
  void addCellValidator(CellValidator... validators);

  /**
   * @return data validate error messages
   */
  List<TemplateValidateMessage> getTemplateErrorMessages();

  /**
   * @return template validate error messages
   */
  List<DataValidateMessage> getDataErrorMessages();

  /**
   * execute validate
   *
   * @return success
   */
  boolean validate();
}
