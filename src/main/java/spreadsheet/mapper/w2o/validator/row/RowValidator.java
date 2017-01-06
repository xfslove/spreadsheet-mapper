package spreadsheet.mapper.w2o.validator.row;


import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.w2o.validator.DependencyValidator;

import java.util.Set;

/**
 * row values validator, after sheet validators.
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface RowValidator extends DependencyValidator {

  /**
   * valid supplied excel row
   *
   * @param row       row
   * @param sheetMeta sheet meta
   * @return true if pass
   */
  boolean valid(Row row, SheetMeta sheetMeta);

  /**
   * @return error message on which fields
   */
  Set<String> getMessageOnFields();
}
