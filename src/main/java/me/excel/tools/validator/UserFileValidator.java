package me.excel.tools.validator;

import me.excel.tools.model.message.ErrorMessage;
import me.excel.tools.validator.cell.CellValidator;
import me.excel.tools.validator.row.RowValidator;
import me.excel.tools.validator.sheet.SheetValidator;
import me.excel.tools.validator.workbook.WorkbookValidator;

import java.util.List;

/**
 * file data validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface UserFileValidator {

  /**
   * @param validators
   * @see WorkbookValidator
   */
  void addWorkbookValidator(WorkbookValidator... validators);

  /**
   * @param validators
   * @see SheetValidator
   */
  void addSheetValidator(SheetValidator... validators);

  /**
   * @param validators
   * @see RowValidator
   */
  void addRowValidator(RowValidator... validators);

  /**
   * @param validators
   * @see CellValidator
   */
  void addCellValidator(CellValidator... validators);

  /**
   * execute validate
   *
   * @return
   */
  boolean validate();

  /**
   * get validate error messages
   *
   * @return
   */
  List<ErrorMessage> getErrorMessages();

}
