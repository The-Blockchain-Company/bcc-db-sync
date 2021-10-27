CREATE FUNCTION migrate() RETURNS void AS $$
BEGIN
  NOTIFY bcc_db_sync_startup, 'db-setup';
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
