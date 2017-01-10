package spreadsheet.mapper.w2o.validator;

import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.message.ErrorMessage;
import spreadsheet.mapper.model.message.MessageWriteStrategies;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.w2o.validator.cell.CellValidator;
import spreadsheet.mapper.w2o.validator.row.RowValidator;
import spreadsheet.mapper.w2o.validator.sheet.SheetValidator;

import java.util.List;

/**
 * sheet validate engine
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface SheetValidateHelper {

  /**
   * @param validators {@link SheetValidator}
   */
  SheetValidateHelper sheetValidator(SheetValidator... validators);

  /**
   * @param validators {@link RowValidator}
   * @see DependencyValidator#getGroup()
   * @see DependencyValidator#getDependsOn()
   */
  SheetValidateHelper rowValidator(RowValidator... validators);

  /**
   * @param validators {@link CellValidator}
   * @see DependencyValidator#getGroup()
   * @see DependencyValidator#getDependsOn()
   */
  SheetValidateHelper cellValidator(CellValidator... validators);

  /**
   * @param sheet {@link Sheet}
   */
  SheetValidateHelper sheet(Sheet sheet);

  /**
   * @param sheetMeta {@link SheetMeta}
   */
  SheetValidateHelper sheetMeta(SheetMeta sheetMeta);

  /**
   * execute valid
   *
   * @return true if pass all
   */
  boolean valid();

  /**
   * <pre>
   * message write strategy of {@link SheetValidator#getErrorMessage()} is {@link MessageWriteStrategies#TEXT_BOX}
   * message write strategy of {@link RowValidator#getErrorMessage()} &amp; {@link CellValidator#getErrorMessage()} is {@link MessageWriteStrategies#COMMENT}
   * </pre>
   *
   * @return list of valid error messages
   */
  List<ErrorMessage> getErrorMessages();
}
