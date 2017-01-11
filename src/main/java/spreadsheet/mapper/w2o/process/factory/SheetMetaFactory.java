package spreadsheet.mapper.w2o.process.factory;

import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.SheetMeta;

/**
 * sheet meta factory
 * <p>
 * Created by hanwen on 2017/1/11.
 */
public interface SheetMetaFactory {

  /**
   * create sheet meta from supplied sheet
   *
   * @param sheet {@link Sheet}
   * @return {@link SheetMeta}
   */
  SheetMeta create(Sheet sheet);
}
