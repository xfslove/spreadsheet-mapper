package spreadsheet.mapper.o2w.compose.converter.buildin;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.o2w.compose.WorkbookComposeException;
import spreadsheet.mapper.o2w.compose.converter.Converter;
import spreadsheet.mapper.utils.FieldUtils;


/**
 * using {@link BeanUtils#getProperty(Object, String)}, this used as default converter
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class BeanUtilsConverter<T> implements Converter<T> {

  private static final Logger LOGGER = LoggerFactory.getLogger(BeanUtilsConverter.class);

  @Override
  public String getValue(T object, Cell cell, FieldMeta fieldMeta) {
    try {
      return BeanUtils.getProperty(object, fieldMeta.getName());
    } catch (NestedNullException e) {
      LOGGER.debug("{} is null", fieldMeta.getName());
      return null;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookComposeException(e);
    }
  }

}
