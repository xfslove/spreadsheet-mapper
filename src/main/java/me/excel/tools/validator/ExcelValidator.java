package me.excel.tools.validator;

import me.excel.tools.validator.cell.CellValidator;
import me.excel.tools.validator.row.RowValidator;
import me.excel.tools.validator.sheet.SheetValidator;
import me.excel.tools.validator.workbook.WorkbookValidator;

/**
 * excel file validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface ExcelValidator {

  /**
   * @param validators workbook validator
   * @see WorkbookValidator
   */
  void addWorkbookValidator(WorkbookValidator... validators);

  /**
   * @param validators sheet validator
   * @see SheetValidator
   */
  void addSheetValidator(SheetValidator... validators);

  /**
   * <pre>
   * multi validators can hold same key({@link DataValidator#getKey()}),
   * if validators has same key, dependsOn({@link DataValidator#getDependsOn()}) will valid all the validators with same key.
   * </pre>
   *
   * @param validators row validator
   * @see RowValidator
   */
  void addRowValidator(RowValidator... validators);

  /**
   * <pre>
   * multi validators can hold same key({@link DataValidator#getKey()}),
   * if validators has same key, dependsOn({@link DataValidator#getDependsOn()}) will valid all the validators with same key.
   * </pre>
   *
   * @param validators cell validator
   * @see CellValidator
   */
  void addCellValidator(CellValidator... validators);

  /**
   * execute valid
   *
   * @return true if passed all validator
   */
  boolean valid();
}
